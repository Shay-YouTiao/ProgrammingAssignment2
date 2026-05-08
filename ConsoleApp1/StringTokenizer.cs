using System;
using System.Collections.Generic;
using System.Text;

namespace HmiParser
{
    // 词法分析器：将HMI文本分割成标记
    public class Token
    {
        public TokenType Type { get; set; }
        public string Value { get; set; }
        public int Line { get; set; }
        public int Column { get; set; }

        public Token(TokenType type, string value, int line, int column)
        {
            Type = type;
            Value = value;
            Line = line;
            Column = column;
        }

        public override string ToString()
        {
            return $"Line {Line}, Col {Column}: {Type} [{Value}]";
        }
    }

    public enum TokenType
    {
        Keyword,
        Identifier,
        String,
        Number,
        Operator,
        Separator,
        Comment,
        EndOfLine,
        EndOfFile,
        Arrow,      // ->
        Assign,     // :=
        OpenBracket, // {
        CloseBracket, // }
        OpenParen,    // (
        CloseParen,   // )
        Semicolon,    // ;
        Comma,        // ,
        Dot           // .
    }

    public class StringTokenizer
    {
        private readonly string _input;
        private int _position;
        private int _line;
        private int _column;
        private readonly List<Token> _tokens;
        private int _tokenIndex;

        private static readonly HashSet<string> Keywords = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
        {
            "screen", "popup", "banner", "addongraphic", "viewproject", "viewfolder",
            "viewcontroller", "shortcut", "datalog", "datalogentry", "namespace",
            "using", "state", "statetable", "statedefinition", "on", "trigger",
            "event", "behavior", "command", "userproperties", "securityroles",
            "true", "false", "visible", "hidden", "if", "else", "for", "while",
            "return", "break", "continue", "and", "or", "not", "xor", "mod",
            "property", "extends", "function", "type", "struct", "enum", "const",
            "alarm", "aoi", "tag", "task", "program", "routine", "input",
            "output", "inout", "action", "transition", "step", "stopstep",
            "converge", "diverge", "simulbranch", "selectbranch", "periodic",
            "continuous", "controller", "gsv", "ssv", "gsv_only", "ssv_only",
            "gsvssv_able", "gsvssv_only", "namedefinition", "defaultstate",
            "view", "parameterconnection", "wire", "instruction", "safety",
            "directedlink", "mathfunction", "motion", "phase", "textbox",
            "artifac", "graphiccelement", "roc_neg", "roc_pos", "unsupported",
            "show", "hide", "forceddisplay", "nullable"
        };

        private static readonly HashSet<string> ObjectTypes = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
        {
            "screen", "popup", "banner", "addongraphic", "viewproject",
            "shortcut", "datalog", "datalogentry", "viewfolder"
        };

        public StringTokenizer(string input)
        {
            _input = RemoveComments(input);
            _position = 0;
            _line = 1;
            _column = 1;
            _tokens = new List<Token>();
            _tokenIndex = 0;
        }

        private string RemoveComments(string input)
        {
            var result = new StringBuilder();
            int i = 0;
            while (i < input.Length)
            {
                if (i + 1 < input.Length && input[i] == '/' && input[i + 1] == '/')
                {
                    // 单行注释
                    while (i < input.Length && input[i] != '\n') i++;
                    if (i < input.Length) result.Append('\n');
                    i++;
                }
                else if (i + 1 < input.Length && input[i] == '/' && input[i + 1] == '*')
                {
                    // 多行注释
                    i += 2;
                    while (i + 1 < input.Length)
                    {
                        if (input[i] == '*' && input[i + 1] == '/')
                        {
                            i += 2;
                            break;
                        }
                        i++;
                    }
                }
                else
                {
                    result.Append(input[i]);
                    i++;
                }
            }
            return result.ToString();
        }

        public List<Token> Tokenize()
        {
            while (_position < _input.Length)
            {
                SkipWhitespace();
                if (_position >= _input.Length) break;

                char c = _input[_position];

                if (c == '{')
                {
                    AddToken(TokenType.OpenBracket, "{");
                    _position++;
                    _column++;
                }
                else if (c == '}')
                {
                    AddToken(TokenType.CloseBracket, "}");
                    _position++;
                    _column++;
                }
                else if (c == '(')
                {
                    AddToken(TokenType.OpenParen, "(");
                    _position++;
                    _column++;
                }
                else if (c == ')')
                {
                    AddToken(TokenType.CloseParen, ")");
                    _position++;
                    _column++;
                }
                else if (c == ';')
                {
                    AddToken(TokenType.Semicolon, ";");
                    _position++;
                    _column++;
                }
                else if (c == ',')
                {
                    AddToken(TokenType.Comma, ",");
                    _position++;
                    _column++;
                }
                else if (c == ':')
                {
                    if (_position + 1 < _input.Length && _input[_position + 1] == '=')
                    {
                        AddToken(TokenType.Assign, ":=");
                        _position += 2;
                        _column += 2;
                    }
                    else if (_position + 1 < _input.Length && _input[_position + 1] == ':')
                    {
                        // Handle :: token
                        ReadIdentifierWithColons();
                    }
                    else
                    {
                        ReadIdentifier();
                    }
                }
                else if (c == '-')
                {
                    if (_position + 1 < _input.Length && _input[_position + 1] == '>')
                    {
                        AddToken(TokenType.Arrow, "->");
                        _position += 2;
                        _column += 2;
                    }
                    else
                    {
                        ReadIdentifier();
                    }
                }
                else if (c == '"' || c == '$')
                {
                    ReadString();
                }
                else if (char.IsLetter(c) || c == '_' || c == '^')
                {
                    ReadIdentifier();
                }
                else if (char.IsDigit(c) || (c == '-' && _position + 1 < _input.Length && char.IsDigit(_input[_position + 1])))
                {
                    ReadNumber();
                }
                else
                {
                    _position++;
                    _column++;
                }
            }

            AddToken(TokenType.EndOfFile, "");
            return _tokens;
        }

        private void SkipWhitespace()
        {
            while (_position < _input.Length && char.IsWhiteSpace(_input[_position]))
            {
                if (_input[_position] == '\n')
                {
                    _line++;
                    _column = 1;
                }
                else
                {
                    _column++;
                }
                _position++;
            }
        }

        private void AddToken(TokenType type, string value)
        {
            _tokens.Add(new Token(type, value, _line, _column));
        }

        private void ReadIdentifier()
        {
            int start = _position;
            int startCol = _column;
            
            while (_position < _input.Length && 
                   (char.IsLetterOrDigit(_input[_position]) || 
                    _input[_position] == '_' || 
                    _input[_position] == '^' ||
                    _input[_position] == '.' ||
                    _input[_position] == '\\' ||
                    _input[_position] == ':' ||
                    _input[_position] == '#'))
            {
                _position++;
                _column++;
            }

            string value = _input.Substring(start, _position - start);
            
            if (Keywords.Contains(value) || ObjectTypes.Contains(value))
            {
                AddToken(TokenType.Keyword, value);
            }
            else
            {
                AddToken(TokenType.Identifier, value);
            }
        }

        private void ReadIdentifierWithColons()
        {
            int start = _position;
            int startCol = _column;
            
            while (_position < _input.Length && 
                   (char.IsLetterOrDigit(_input[_position]) || 
                    _input[_position] == '_' || 
                    _input[_position] == '^' ||
                    _input[_position] == '.' ||
                    _input[_position] == '\\' ||
                    _input[_position] == ':' ||
                    _input[_position] == '#'))
            {
                _position++;
                _column++;
            }

            string value = _input.Substring(start, _position - start);
            AddToken(TokenType.Identifier, value);
        }

        private void ReadString()
        {
            char quoteChar = _input[_position];
            int start = _position;
            int startCol = _column;
            _position++; // skip opening quote
            _column++;

            var sb = new StringBuilder();
            
            while (_position < _input.Length && _input[_position] != quoteChar)
            {
                if (_input[_position] == '\\' && _position + 1 < _input.Length)
                {
                    _position++;
                    _column++;
                    switch (_input[_position])
                    {
                        case 'n': sb.Append('\n'); break;
                        case 't': sb.Append('\t'); break;
                        case 'r': sb.Append('\r'); break;
                        case '\\': sb.Append('\\'); break;
                        case '"': sb.Append('"'); break;
                        case '$': sb.Append('$'); break;
                        default: sb.Append(_input[_position]); break;
                    }
                }
                else
                {
                    sb.Append(_input[_position]);
                }
                _position++;
                _column++;
            }

            if (_position < _input.Length && _input[_position] == quoteChar)
            {
                _position++; // skip closing quote
                _column++;
            }

            string strValue = sb.ToString();
            AddToken(TokenType.String, strValue);
        }

        private void ReadNumber()
        {
            int start = _position;
            int startCol = _column;
            
            if (_input[_position] == '-')
            {
                _position++;
                _column++;
            }

            while (_position < _input.Length && 
                   (char.IsDigit(_input[_position]) || _input[_position] == '.'))
            {
                _position++;
                _column++;
            }

            string value = _input.Substring(start, _position - start);
            AddToken(TokenType.Number, value);
        }

        public Token PeekToken()
        {
            if (_tokenIndex < _tokens.Count)
                return _tokens[_tokenIndex];
            return new Token(TokenType.EndOfFile, "", _line, _column);
        }

        public Token NextToken()
        {
            if (_tokenIndex < _tokens.Count)
                return _tokens[_tokenIndex++];
            return new Token(TokenType.EndOfFile, "", _line, _column);
        }

        public void Reset()
        {
            _tokenIndex = 0;
        }
    }
}