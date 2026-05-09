using System;
using System.Collections.Generic;
using System.Linq;

namespace HmiParser
{
    /// <summary>
    /// 语法分析器
    /// </summary>
    public class Parser
    {
        private readonly Lexer _lexer;
        private Token _currentToken;

        private static readonly HashSet<string> GraphicElementTypes = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
        {
            "button", "textdisplay", "bargraph1", "numericdisplay", "numericinput",
            "image", "circle", "rectangle", "line", "polygon", "arc",
            "alarmstatusindicator", "networkstatusindicator",
            "datalogstatusindicator", "projecteventsstatusindicator",
            "automaticdiagnosticsstatusindicator"
        };

        public Parser(string text)
        {
            _lexer = new Lexer(text);
            _currentToken = _lexer.GetNextToken();
        }

        private void Eat(TokenType tokenType)
        {
            if (_currentToken.Type == tokenType)
            {
                _currentToken = _lexer.GetNextToken();
            }
            else
            {
                throw new SyntaxErrorException(
                    $"Unexpected token: '{_currentToken.Value}' " +
                    $"(expected {tokenType}) at line {_currentToken.Line}, column {_currentToken.Column}");
            }
        }

        private void Eat()
        {
            _currentToken = _lexer.GetNextToken();
        }

        /// <summary>
        /// 解析入口
        /// </summary>
        public HmiObject Parse()
        {
            // 跳过 namespace 和 using 声明
            while (_currentToken.Type == TokenType.Keyword || _currentToken.Type == TokenType.Identifier)
            {
                if (_currentToken.Value.Equals("namespace", StringComparison.OrdinalIgnoreCase))
                {
                    Eat(TokenType.Keyword);
                    // 读取命名空间名称
                    while (_currentToken.Type == TokenType.Identifier ||
                           (_currentToken.Type == TokenType.Keyword && _currentToken.Value == "::"))
                    {
                        Eat();
                    }
                    Eat(TokenType.Semicolon);
                }
                else if (_currentToken.Value.Equals("using", StringComparison.OrdinalIgnoreCase))
                {
                    Eat(TokenType.Keyword);
                    Eat(TokenType.Identifier);
                    Eat(TokenType.Semicolon);
                }
                else
                {
                    break;
                }
            }

            // 解析主体对象
            return ParseObject();
        }

        private HmiObject ParseObject()
        {
            string objType = _currentToken.Value?.ToLower() ?? "";

            switch (objType)
            {
                case "screen":
                    return ParseScreen();
                case "popup":
                    return ParsePopup();
                case "addongraphic":
                    return ParseAddOnGraphic();
                case "viewproject":
                    return ParseViewProject();
                case "banner":
                    return ParseScreen();
                case "datalog":
                    return ParseDataLog();
                case "shortcut":
                    return ParseShortcut();
                case "viewfolder":
                    return ParseViewFolder();
                default:
                    return ParseGenericObject();
            }
        }

        private Screen ParseScreen()
        {
            Eat(); // screen keyword

            string name = "";
            if (_currentToken.Type == TokenType.Identifier || _currentToken.Type == TokenType.Keyword)
            {
                name = _currentToken.Value;
                Eat();
            }

            var screen = new Screen { Name = name };
            ParseProperties(screen);
            return screen;
        }

        private Popup ParsePopup()
        {
            Eat(); // popup keyword

            string name = "";
            if (_currentToken.Type == TokenType.Identifier || _currentToken.Type == TokenType.Keyword)
            {
                name = _currentToken.Value;
                Eat();
            }

            var popup = new Popup { Name = name };
            ParseProperties(popup);
            return popup;
        }

        private AddOnGraphic ParseAddOnGraphic()
        {
            Eat(); // addongraphic keyword

            string name = "";
            if (_currentToken.Type == TokenType.Identifier || _currentToken.Type == TokenType.Keyword)
            {
                name = _currentToken.Value;
                Eat();
            }

            var aog = new AddOnGraphic { Name = name };
            ParseProperties(aog);
            return aog;
        }

        private ViewProject ParseViewProject()
        {
            Eat(); // viewproject keyword

            string name = "";
            if (_currentToken.Type == TokenType.Identifier || _currentToken.Type == TokenType.Keyword)
            {
                name = _currentToken.Value;
                Eat();
            }

            var project = new ViewProject { Name = name };
            ParseProperties(project);
            return project;
        }

        private HmiObject ParseDataLog()
        {
            Eat(); // datalog keyword

            string name = "";
            if (_currentToken.Type == TokenType.Identifier || _currentToken.Type == TokenType.Keyword)
            {
                name = _currentToken.Value;
                Eat();
            }

            var datalog = new HmiObject { Name = name, ObjectType = "DataLog" };
            ParseProperties(datalog);
            return datalog;
        }

        private HmiObject ParseShortcut()
        {
            Eat(); // shortcut keyword

            string name = "";
            if (_currentToken.Type == TokenType.Identifier || _currentToken.Type == TokenType.Keyword)
            {
                name = _currentToken.Value;
                Eat();
            }

            var shortcut = new HmiObject { Name = name, ObjectType = "Shortcut" };

            // 检查是否有参数
            if (_currentToken.Type == TokenType.LParen)
            {
                Eat(TokenType.LParen);
                SkipUntilRParen();
                Eat(TokenType.RParen);
            }

            ParseProperties(shortcut);
            return shortcut;
        }

        private HmiObject ParseViewFolder()
        {
            Eat(); // viewfolder keyword

            string name = "";
            if (_currentToken.Type == TokenType.Identifier || _currentToken.Type == TokenType.Keyword)
            {
                name = _currentToken.Value;
                Eat();
            }

            var folder = new HmiObject { Name = name, ObjectType = "ViewFolder" };
            ParseProperties(folder);
            return folder;
        }

        private HmiObject ParseGenericObject()
        {
            string typeName = _currentToken.Value;
            Eat();

            string name = "";
            if (_currentToken.Type == TokenType.Identifier || _currentToken.Type == TokenType.Keyword)
            {
                name = _currentToken.Value;
                Eat();
            }

            // 检查是否有参数
            if (_currentToken.Type == TokenType.LParen)
            {
                Eat(TokenType.LParen);
                SkipUntilRParen();
                Eat(TokenType.RParen);
            }

            var obj = new HmiObject { Name = name, ObjectType = typeName };
            ParseProperties(obj);
            return obj;
        }

        private void ParseProperties(HmiObject obj)
        {
            Eat(TokenType.LBrace);

            while (_currentToken.Type != TokenType.RBrace && _currentToken.Type != TokenType.EOF)
            {
                ParsePropertyOrElement(obj);
            }

            Eat(TokenType.RBrace);
        }

        private void ParsePropertyOrElement(HmiObject obj)
        {
            if (_currentToken.Type == TokenType.EOF)
                return;

            string identifier = _currentToken.Value;
            int startLine = _currentToken.Line;

            // 检查是否是子对象（关键字后跟 { 或标识符后跟 {）
            if (_currentToken.Type == TokenType.Keyword)
            {
                // 可能是子对象
                if (identifier.Equals("border", StringComparison.OrdinalIgnoreCase))
                {
                    var border = ParseBorder();
                    obj.Children.Add(border);
                    return;
                }
                else if (identifier.Equals("borderstyle", StringComparison.OrdinalIgnoreCase))
                {
                    var style = ParseBorderStyle();
                    obj.Children.Add(style);
                    return;
                }
                else if (identifier.Equals("securityroles", StringComparison.OrdinalIgnoreCase))
                {
                    var roles = ParseSecurityRoles();
                    obj.Children.Add(roles);
                    return;
                }
                else if (identifier.Equals("userproperties", StringComparison.OrdinalIgnoreCase))
                {
                    var props = ParseUserProperties();
                    obj.Children.Add(props);
                    return;
                }
                else if (identifier.Equals("statetable", StringComparison.OrdinalIgnoreCase))
                {
                    var stateTable = ParseStateTable();
                    obj.Children.Add(stateTable);
                    return;
                }
                else if (GraphicElementTypes.Contains(identifier))
                {
                    var element = ParseGraphicElement(identifier);
                    obj.Children.Add(element);
                    return;
                }
            }

            // 处理属性赋值或子对象
            Eat();

            if (_currentToken.Type == TokenType.Assign)
            {
                // 属性赋值
                Eat(TokenType.Assign);
                object value = ParseValue();
                Eat(TokenType.Semicolon);
                SetProperty(obj, identifier, value);
            }
            else if (_currentToken.Type == TokenType.LBrace)
            {
                // 子对象
                var child = new HmiObject { Name = identifier, ObjectType = identifier };
                ParseProperties(child);
                obj.Children.Add(child);
            }
            else if (_currentToken.Type == TokenType.LParen)
            {
                // 带参数的子对象
                Eat(TokenType.LParen);
                SkipUntilRParen();
                Eat(TokenType.RParen);

                var child = new HmiObject { Name = identifier, ObjectType = identifier };
                ParseProperties(child);
                obj.Children.Add(child);
            }
            else if (_currentToken.Type == TokenType.Semicolon)
            {
                // 无值的属性（如标志）
                Eat(TokenType.Semicolon);
                SetProperty(obj, identifier, true);
            }
            else if (_currentToken.Type == TokenType.Keyword || _currentToken.Type == TokenType.Identifier)
            {
                // 可能是对象实例（类型名 实例名）
                string instanceType = identifier;
                string instanceName = _currentToken.Value;
                Eat();

                var child = new HmiObject { Name = instanceName, ObjectType = instanceType };

                // 检查是否有参数
                if (_currentToken.Type == TokenType.LParen)
                {
                    Eat(TokenType.LParen);
                    ParseInstanceParameters(child);
                    Eat(TokenType.RParen);
                }

                ParseProperties(child);
                obj.Children.Add(child);
            }
            else
            {
                // 跳过未知
                if (_currentToken.Type == TokenType.Semicolon)
                    Eat(TokenType.Semicolon);
            }
        }

        private HmiObject ParseBorder()
        {
            Eat(); // border keyword
            var border = new HmiObject { Name = "Border", ObjectType = "Border" };
            ParseProperties(border);
            return border;
        }

        private HmiObject ParseBorderStyle()
        {
            Eat(); // borderstyle keyword
            var style = new HmiObject { Name = "BorderStyle", ObjectType = "BorderStyle" };
            ParseProperties(style);
            return style;
        }

        private HmiObject ParseSecurityRoles()
        {
            Eat(); // securityroles keyword
            var roles = new HmiObject { Name = "SecurityRoles", ObjectType = "SecurityRoles" };
            ParseProperties(roles);
            return roles;
        }

        private HmiObject ParseUserProperties()
        {
            Eat(); // userproperties keyword
            var props = new HmiObject { Name = "UserProperties", ObjectType = "UserProperties" };
            ParseProperties(props);
            return props;
        }

        private HmiObject ParseStateTable()
        {
            Eat(); // statetable keyword

            string name = "";
            if (_currentToken.Type == TokenType.Identifier || _currentToken.Type == TokenType.Keyword)
            {
                name = _currentToken.Value;
                Eat();
            }

            var stateTable = new HmiObject { Name = name, ObjectType = "StateTable" };
            ParseProperties(stateTable);
            return stateTable;
        }

        private HmiObject ParseGraphicElement(string elementType)
        {
            Eat(); // element type

            string name = "";
            if (_currentToken.Type == TokenType.Identifier || _currentToken.Type == TokenType.Keyword)
            {
                name = _currentToken.Value;
                Eat();
            }

            var element = new HmiObject { Name = name, ObjectType = elementType };

            // 检查是否有参数
            if (_currentToken.Type == TokenType.LParen)
            {
                Eat(TokenType.LParen);
                ParseInstanceParameters(element);
                Eat(TokenType.RParen);
            }

            ParseProperties(element);
            return element;
        }

        private void ParseInstanceParameters(HmiObject obj)
        {
            // 解析实例参数 (key := value, ...)
            while (_currentToken.Type != TokenType.RParen && _currentToken.Type != TokenType.EOF)
            {
                if (_currentToken.Type == TokenType.Identifier || _currentToken.Type == TokenType.Keyword)
                {
                    string key = _currentToken.Value;
                    Eat();

                    if (_currentToken.Type == TokenType.Assign)
                    {
                        Eat(TokenType.Assign);
                        object value = ParseValue();
                        SetProperty(obj, key, value);
                    }
                }
                else
                {
                    Eat();
                }

                // 跳过逗号
                if (_currentToken.Value == ",")
                {
                    Eat();
                }
            }
        }

        private object ParseValue()
        {
            if (_currentToken.Type == TokenType.String)
            {
                string value = _currentToken.Value;
                Eat(TokenType.String);

                // 检查是否有绑定
                if (_currentToken.Type == TokenType.Bind)
                {
                    Eat(TokenType.Bind);
                    object boundValue = ParseValue();
                    return new Dictionary<string, object>
                    {
                        ["value"] = value,
                        ["boundTo"] = boundValue
                    };
                }

                return value;
            }
            else if (_currentToken.Type == TokenType.Number)
            {
                string numberStr = _currentToken.Value;
                Eat(TokenType.Number);

                if (numberStr.Contains("."))
                {
                    return double.Parse(numberStr, System.Globalization.CultureInfo.InvariantCulture);
                }
                else
                {
                    return int.Parse(numberStr);
                }
            }
            else if (_currentToken.Type == TokenType.Identifier || _currentToken.Type == TokenType.Keyword)
            {
                string value = _currentToken.Value;
                Eat();

                // 处理枚举值（如 EnumAlignment.HCENTER_VCENTER）
                while (_currentToken.Type == TokenType.Identifier &&
                       _currentToken.Value.StartsWith("."))
                {
                    value += _currentToken.Value;
                    Eat();
                }

                // 处理布尔值
                if (value.Equals("true", StringComparison.OrdinalIgnoreCase))
                    return true;
                if (value.Equals("false", StringComparison.OrdinalIgnoreCase))
                    return false;

                // 检查是否有绑定
                if (_currentToken.Type == TokenType.Bind)
                {
                    Eat(TokenType.Bind);
                    object boundValue = ParseValue();
                    return new Dictionary<string, object>
                    {
                        ["value"] = value,
                        ["boundTo"] = boundValue
                    };
                }

                return value;
            }
            else
            {
                // 未知值
                string value = _currentToken.Value;
                Eat();
                return value;
            }
        }

        private void SkipUntilRParen()
        {
            int depth = 1;
            while (_currentToken.Type != TokenType.EOF && depth > 0)
            {
                if (_currentToken.Type == TokenType.LParen)
                    depth++;
                else if (_currentToken.Type == TokenType.RParen)
                    depth--;

                if (depth > 0)
                    Eat();
            }
        }

        private void SetProperty(HmiObject obj, string key, object value)
        {
            // 清理 key（移除 ^ 前缀）
            string cleanKey = key.TrimStart('^');

            // 映射常见属性名
            string mappedKey = PropertyMapper.MapPropertyName(cleanKey);
            obj.Properties[mappedKey] = value;
        }
    }
}