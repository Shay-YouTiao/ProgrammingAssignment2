using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json;

/// <summary>
/// Studio 5000 View Designer .hmi 文件解析器
/// 根据 9324-RM001 参考手册实现
/// </summary>
public static class HmiFileParser
{
    public static string ParseHmiToJson(string hmiContent)
    {
        var tokens = Tokenize(hmiContent);
        var parser = new Parser(tokens);
        var root = parser.ParseFile();
        return JsonSerializer.Serialize(root, new JsonSerializerOptions { WriteIndented = true });
    }

    // ---------- 词法分析 ----------
    enum TokenType { Identifier, String, Number, Bool, Assign, Arrow, Semicolon, Comma, Dot, OpenBrace, CloseBrace, OpenParen, CloseParen, Eof }

    class Token
    {
        public TokenType Type;
        public string Value;
        public int Line;
        public int Column;
    }

    static List<Token> Tokenize(string input)
    {
        var tokens = new List<Token>();
        int i = 0, line = 1, col = 1;
        while (i < input.Length)
        {
            char c = input[i];
            // 空白
            if (char.IsWhiteSpace(c))
            {
                if (c == '\n') { line++; col = 1; }
                else col++;
                i++; continue;
            }
            // 注释 //
            if (c == '/' && i + 1 < input.Length && input[i + 1] == '/')
            {
                i += 2; col += 2;
                while (i < input.Length && input[i] != '\n') { i++; col++; }
                continue;
            }
            // 注释 /* */
            if (c == '/' && i + 1 < input.Length && input[i + 1] == '*')
            {
                i += 2; col += 2;
                while (i < input.Length - 1 && !(input[i] == '*' && input[i + 1] == '/'))
                {
                    if (input[i] == '\n') { line++; col = 1; }
                    else col++;
                    i++;
                }
                if (i < input.Length) { i += 2; col += 2; }
                continue;
            }
            // 字符串
            if (c == '"')
            {
                int startLine = line, startCol = col;
                i++; col++;
                var sb = new StringBuilder();
                while (i < input.Length)
                {
                    if (input[i] == '"') { i++; col++; break; }
                    else if (input[i] == '$' && i + 1 < input.Length && input[i + 1] == '"') // $" 转义为双引号
                    {
                        sb.Append('"');
                        i += 2; col += 2;
                    }
                    else if (input[i] == '$' && i + 1 < input.Length && input[i + 1] == '$') // $$ 转义为 $
                    {
                        sb.Append('$');
                        i += 2; col += 2;
                    }
                    else
                    {
                        if (input[i] == '\n') { line++; col = 1; }
                        else col++;
                        sb.Append(input[i]);
                        i++;
                    }
                }
                tokens.Add(new Token { Type = TokenType.String, Value = sb.ToString(), Line = startLine, Column = startCol });
                continue;
            }
            // 数字（整数或浮点）
            if (char.IsDigit(c) || (c == '-' && i + 1 < input.Length && char.IsDigit(input[i + 1])))
            {
                int start = i, startLine = line, startCol = col;
                i++; col++;
                while (i < input.Length && (char.IsDigit(input[i]) || input[i] == '.')) { i++; col++; }
                tokens.Add(new Token { Type = TokenType.Number, Value = input.Substring(start, i - start), Line = startLine, Column = startCol });
                continue;
            }
            // 标识符或关键字 / 以 ^ 开头
            if (char.IsLetter(c) || c == '_' || c == '^')
            {
                int start = i, startLine = line, startCol = col;
                i++; col++;
                while (i < input.Length && (char.IsLetterOrDigit(input[i]) || input[i] == '_')) { i++; col++; }
                string word = input.Substring(start, i - start);
                if (word.Equals("true", StringComparison.OrdinalIgnoreCase) || word.Equals("false", StringComparison.OrdinalIgnoreCase))
                    tokens.Add(new Token { Type = TokenType.Bool, Value = word.ToLower(), Line = startLine, Column = startCol });
                else
                    tokens.Add(new Token { Type = TokenType.Identifier, Value = word, Line = startLine, Column = startCol });
                continue;
            }
            // 符号
            switch (c)
            {
                case ':': if (i + 1 < input.Length && input[i + 1] == '=') { tokens.Add(new Token { Type = TokenType.Assign, Value = ":=", Line = line, Column = col }); i += 2; col += 2; } else throw new Exception($"非法字符 ':' at {line}:{col}"); break;
                case '-': if (i + 1 < input.Length && input[i + 1] == '>') { tokens.Add(new Token { Type = TokenType.Arrow, Value = "->", Line = line, Column = col }); i += 2; col += 2; } else throw new Exception($"非法字符 '-' at {line}:{col}"); break;
                case ';': tokens.Add(new Token { Type = TokenType.Semicolon, Value = ";", Line = line, Column = col }); i++; col++; break;
                case ',': tokens.Add(new Token { Type = TokenType.Comma, Value = ",", Line = line, Column = col }); i++; col++; break;
                case '.': tokens.Add(new Token { Type = TokenType.Dot, Value = ".", Line = line, Column = col }); i++; col++; break;
                case '{': tokens.Add(new Token { Type = TokenType.OpenBrace, Value = "{", Line = line, Column = col }); i++; col++; break;
                case '}': tokens.Add(new Token { Type = TokenType.CloseBrace, Value = "}", Line = line, Column = col }); i++; col++; break;
                case '(': tokens.Add(new Token { Type = TokenType.OpenParen, Value = "(", Line = line, Column = col }); i++; col++; break;
                case ')': tokens.Add(new Token { Type = TokenType.CloseParen, Value = ")", Line = line, Column = col }); i++; col++; break;
                default: throw new Exception($"未识别的字符 '{c}' at {line}:{col}");
            }
        }
        tokens.Add(new Token { Type = TokenType.Eof });
        return tokens;
    }

    // ---------- 语法分析 ----------
    class Parser
    {
        private readonly List<Token> tokens;
        private int pos;

        public Parser(List<Token> tokens)
        {
            this.tokens = tokens;
            pos = 0;
        }

        Token Current => tokens[pos];
        Token Next => pos + 1 < tokens.Count ? tokens[pos + 1] : new Token { Type = TokenType.Eof };
        void Advance() { if (pos < tokens.Count) pos++; }
        void Expect(TokenType type)
        {
            if (Current.Type != type) throw new Exception($"期望 {type}，但遇到 {Current.Type} '{Current.Value}' at {Current.Line}:{Current.Column}");
            Advance();
        }

        // 顶层解析
        public JsonElement ParseFile()
        {
            var fileObj = new Dictionary<string, object>();
            // namespace
            if (Current.Type == TokenType.Identifier && Current.Value.Equals("namespace", StringComparison.OrdinalIgnoreCase))
            {
                Advance();
                string ns = Current.Value; Advance();
                Expect(TokenType.Semicolon);
                fileObj["namespace"] = ns;
            }
            // using
            if (Current.Type == TokenType.Identifier && Current.Value.Equals("using", StringComparison.OrdinalIgnoreCase))
            {
                Advance();
                string us = Current.Value; Advance();
                Expect(TokenType.Semicolon);
                fileObj["using"] = us;
            }
            // 注释设备类型（/* ... */ 已在词法分析中忽略，但可能残留 HMI Device Type 标识符? 文档中是用注释，所以跳过）
            // 顶层对象
            var topElement = ParseObject();
            fileObj["element"] = topElement;

            return JsonSerializer.SerializeToElement(fileObj, new JsonSerializerOptions { WriteIndented = true });
        }

        // 解析一个对象：Type [Name] [(参数)] { ... }
        object ParseObject()
        {
            string type = Current.Value; Advance();
            string name = null;
            Dictionary<string, object> parameters = null;

            // 可选名称
            if (Current.Type == TokenType.Identifier && (Next.Type == TokenType.OpenBrace || Next.Type == TokenType.OpenParen))
            {
                name = Current.Value; Advance();
            }
            // 可选参数列表
            if (Current.Type == TokenType.OpenParen)
            {
                parameters = ParseParameterList();
            }
            Expect(TokenType.OpenBrace);
            var properties = new Dictionary<string, object>();
            var children = new List<object>();
            while (Current.Type != TokenType.CloseBrace && Current.Type != TokenType.Eof)
            {
                ParseBlockEntry(properties, children);
            }
            Expect(TokenType.CloseBrace);

            var obj = new Dictionary<string, object>
            {
                { "type", type },
            };
            if (name != null) obj["name"] = name;
            // 合并参数到属性（参数也是属性）
            if (parameters != null)
            {
                foreach (var kv in parameters)
                    obj[kv.Key] = kv.Value;
            }
            if (properties.Count > 0)
            {
                foreach (var kv in properties)
                    obj[kv.Key] = kv.Value;
            }
            if (children.Count > 0)
                obj["children"] = children;
            return obj;
        }

        // 参数列表：( id := expr , ... )
        Dictionary<string, object> ParseParameterList()
        {
            Expect(TokenType.OpenParen);
            var dict = new Dictionary<string, object>();
            if (Current.Type != TokenType.CloseParen)
            {
                while (true)
                {
                    string paramName = Current.Value;
                    Expect(TokenType.Identifier);
                    Expect(TokenType.Assign);
                    object value = ParseExpressionValue();
                    dict[paramName] = value;
                    if (Current.Type == TokenType.Comma) { Advance(); }
                    else break;
                }
            }
            Expect(TokenType.CloseParen);
            return dict;
        }

        // 块内条目：属性赋值 或 子对象定义
        void ParseBlockEntry(Dictionary<string, object> properties, List<object> children)
        {
            if (Current.Type != TokenType.Identifier)
                throw new Exception($"块内期望属性或对象，但遇到 {Current.Type} '{Current.Value}'");

            // 前瞻判断是属性还是对象
            int saved = pos;
            string first = Current.Value; Advance();
            if (Current.Type == TokenType.Assign)
            {
                // 属性赋值
                Advance(); // 跳过 :=
                object leftVal = ParseExpressionValue();
                object value;
                if (Current.Type == TokenType.Arrow)
                {
                    Advance();
                    object rightVal = ParseExpressionValue();
                    value = new Dictionary<string, object>
                    {
                        { "initial", leftVal },
                        { "bound", rightVal }
                    };
                }
                else
                {
                    value = leftVal;
                }
                Expect(TokenType.Semicolon);
                properties[first] = value;
            }
            else
            {
                // 对象定义：回退并解析对象
                pos = saved;
                children.Add(ParseObject());
            }
        }

        // 解析表达式的值（基本值，可能是数字/字符串/布尔/枚举路径）
        object ParseExpressionValue()
        {
            if (Current.Type == TokenType.Number)
            {
                string s = Current.Value;
                Advance();
                if (s.Contains(".")) return double.Parse(s, System.Globalization.CultureInfo.InvariantCulture);
                return long.Parse(s);
            }
            else if (Current.Type == TokenType.String)
            {
                string s = Current.Value; Advance();
                return s;
            }
            else if (Current.Type == TokenType.Bool)
            {
                bool b = Current.Value == "true"; Advance();
                return b;
            }
            else if (Current.Type == TokenType.Identifier)
            {
                // 枚举引用 或 简单的标识符
                StringBuilder sb = new StringBuilder(Current.Value);
                Advance();
                while (Current.Type == TokenType.Dot)
                {
                    sb.Append('.');
                    Advance();
                    if (Current.Type == TokenType.Identifier)
                    {
                        sb.Append(Current.Value);
                        Advance();
                    }
                    else throw new Exception($"期望标识符在 '.' 之后 at {Current.Line}:{Current.Column}");
                }
                return sb.ToString(); // 枚举引用以字符串形式保存
            }
            else
            {
                throw new Exception($"无法解析表达式值，意外 token {Current.Type} '{Current.Value}'");
            }
        }
    }
}

// ---------- 使用示例 ----------
class Program
{
    static void Main(string[] args)
    {
        // 示例 .hmi 内容（取自手册）
        string sampleHmi = @"
namespace ViewDesigner;
using HMICatalog;
/* HMI Device Type: 15"" PanelView 5510 */
ViewProject SampleProject{
    HomeScreen := ""Screen_001"";
}

Screen MyScreen {
    ShowDefaultBanner := true;
    FillColor := ""#ffffff"";
    UpdateRate := EnumUpdateRate._500_ms;
    Button Sample_Button {
        Text := ""Sample Button"";
        CornerRadius := 3;
        Enabled := true;
        ^Visible := true;
        BehaviorNavigateToScreen Behavior_001 {
            screenName := ""Navigation Menu\Settings"";
            Key := EnumBezelKeys.VK_NONE;
        }
    }
}
";

        string json = HmiFileParser.ParseHmiToJson(sampleHmi);
        Console.WriteLine(json);
    }
}