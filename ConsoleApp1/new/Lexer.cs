using System;
using System.Collections.Generic;
using System.Text;

namespace HmiParser
{
    /// <summary>
    /// 词法分析器
    /// </summary>
    public class Lexer
    {
        private readonly string _text;
        private int _pos;
        private int _line = 1;
        private int _column = 1;
        private char? _currentChar;

        private static readonly HashSet<string> ReservedKeywords = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
        {
            "screen", "popup", "banner", "addongraphic", "shortcut",
            "viewproject", "viewfolder", "viewapplication",
            "namespace", "using", "true", "false",
            "button", "textdisplay", "bargraph1",
            "statetable", "state", "defaultstate",
            "behavior", "command", "event",
            "securityroles", "userproperties",
            "border", "borderstyle",
            "aog", "datalog", "datalogentry",
            "viewcontroller", "on", "off",
            "nod", "of", "or", "repeat", "then", "to", "until", "while",
            "by", "case", "else", "elsif", "end_case", "end_for",
            "end_if", "end_repeat", "end_while", "exit", "for", "if", "mod"
        };

        public Lexer(string text)
        {
            _text = text;
            _pos = 0;
            _currentChar = _text.Length > 0 ? _text[0] : null;
        }

        private void Advance()
        {
            if (_currentChar == '\n')
            {
                _line++;
                _column = 1;
            }
            else
            {
                _column++;
            }

            _pos++;
            _currentChar = _pos < _text.Length ? _text[_pos] : null;
        }

        private char? Peek()
        {
            int peekPos = _pos + 1;
            return peekPos < _text.Length ? _text[peekPos] : null;
        }

        private void SkipWhitespace()
        {
            while (_currentChar.HasValue && char.IsWhiteSpace(_currentChar.Value))
            {
                Advance();
            }
        }

        private void SkipComment()
        {
            if (_currentChar == '/' && Peek() == '/')
            {
                // 单行注释
                while (_currentChar.HasValue && _currentChar != '\n')
                {
                    Advance();
                }
            }
            else if (_currentChar == '/' && Peek() == '*')
            {
                // 多行注释
                Advance(); // /
                Advance(); // *
                while (_currentChar.HasValue)
                {
                    if (_currentChar == '*' && Peek() == '/')
                    {
                        Advance();
                        Advance();
                        break;
                    }
                    Advance();
                }
            }
        }

        private string ReadString()
        {
            var sb = new StringBuilder();
            Advance(); // 跳过开始的引号

            while (_currentChar.HasValue && _currentChar != '"')
            {
                if (_currentChar == '\\' && Peek() == '"')
                {
                    Advance();
                    sb.Append('"');
                }
                else
                {
                    sb.Append(_currentChar.Value);
                }
                Advance();
            }

            if (_currentChar == '"')
            {
                Advance(); // 跳过结束的引号
            }

            return sb.ToString();
        }

        private string ReadIdentifier()
        {
            var sb = new StringBuilder();

            while (_currentChar.HasValue && 
                   (char.IsLetterOrDigit(_currentChar.Value) || 
                    _currentChar == '_' || _currentChar == '^' || 
                    _currentChar == '$' || _currentChar == '.'))
            {
                // 处理 .. 的情况
                if (_currentChar == '.' && Peek() == '.')
                {
                    break;
                }
                sb.Append(_currentChar.Value);
                Advance();
            }

            return sb.ToString();
        }

        private string ReadNumber()
        {
            var sb = new StringBuilder();

            while (_currentChar.HasValue && 
                   (char.IsDigit(_currentChar.Value) || _currentChar == '.'))
            {
                sb.Append(_currentChar.Value);
                Advance();
            }

            return sb.ToString();
        }

        public Token GetNextToken()
        {
            while (_currentChar.HasValue)
            {
                // 跳过空白
                if (char.IsWhiteSpace(_currentChar.Value))
                {
                    SkipWhitespace();
                    continue;
                }

                // 跳过注释
                if (_currentChar == '/' && Peek() is char next && (next == '/' || next == '*'))
                {
                    SkipComment();
                    continue;
                }

                // 字符串
                if (_currentChar == '"')
                {
                    string value = ReadString();
                    return new Token(TokenType.String, value, _line, _column);
                }

                // 赋值操作符 :=
                if (_currentChar == ':' && Peek() == '=')
                {
                    Advance();
                    Advance();
                    return new Token(TokenType.Assign, ":=", _line, _column);
                }

                // 绑定操作符 ->
                if (_currentChar == '-' && Peek() == '>')
                {
                    Advance();
                    Advance();
                    return new Token(TokenType.Bind, "->", _line, _column);
                }

                // 分号
                if (_currentChar == ';')
                {
                    Advance();
                    return new Token(TokenType.Semicolon, ";", _line, _column);
                }

                // 左大括号
                if (_currentChar == '{')
                {
                    Advance();
                    return new Token(TokenType.LBrace, "{", _line, _column);
                }

                // 右大括号
                if (_currentChar == '}')
                {
                    Advance();
                    return new Token(TokenType.RBrace, "}", _line, _column);
                }

                // 左括号
                if (_currentChar == '(')
                {
                    Advance();
                    return new Token(TokenType.LParen, "(", _line, _column);
                }

                // 右括号
                if (_currentChar == ')')
                {
                    Advance();
                    return new Token(TokenType.RParen, ")", _line, _column);
                }

                // 数字
                if (char.IsDigit(_currentChar.Value) || _currentChar == '.')
                {
                    string value = ReadNumber();
                    return new Token(TokenType.Number, value, _line, _column);
                }

                // 标识符或关键字
                if (char.IsLetter(_currentChar.Value) || _currentChar == '_')
                {
                    string value = ReadIdentifier();
                    TokenType type = ReservedKeywords.Contains(value) ? TokenType.Keyword : TokenType.Identifier;
                    return new Token(type, value, _line, _column);
                }

                // 跳过未知字符
                Advance();
            }

            return new Token(TokenType.EOF, "", _line, _column);
        }
    }
}