using System;
using System.Collections.Generic;
using System.Linq;

namespace HmiParser
{
    // 语法分析器：将标记流解析为数据结构
    public class HmiParser
    {
        private StringTokenizer _tokenizer;
        private Token _currentToken;

        public HmiProject ParseFile(string content)
        {
            _tokenizer = new StringTokenizer(content);
            _tokenizer.Tokenize();
            _tokenizer.Reset();
            _currentToken = _tokenizer.NextToken();

            return ParseProject();
        }

        private HmiProject ParseProject()
        {
            var project = new HmiProject();
            var viewProject = new ViewProject();

            // 解析命名空间
            ExpectKeyword("namespace");
            if (_currentToken.Type == TokenType.Identifier)
            {
                project.Namespace = _currentToken.Value;
                ConsumeCurrent();
                ExpectTokenType(TokenType.Semicolon);
            }

            // 解析using
            if (_currentToken.Type == TokenType.Keyword && 
                string.Equals(_currentToken.Value, "using", StringComparison.OrdinalIgnoreCase))
            {
                ConsumeCurrent();
                if (_currentToken.Type == TokenType.Identifier)
                {
                    project.Using = _currentToken.Value;
                    ConsumeCurrent();
                    ExpectTokenType(TokenType.Semicolon);
                }
            }

            // 解析主要内容
            while (_currentToken.Type != TokenType.EndOfFile)
            {
                if (_currentToken.Type == TokenType.Keyword)
                {
                    string keyword = _currentToken.Value.ToLower();
                    
                    switch (keyword)
                    {
                        case "viewproject":
                            ParseViewProject(viewProject);
                            break;
                        case "screen":
                            viewProject.Screens.Add(ParseObject("screen"));
                            break;
                        case "popup":
                            viewProject.Popups.Add(ParseObject("popup"));
                            break;
                        case "addongraphic":
                            viewProject.AddOnGraphics.Add(ParseObject("addongraphic"));
                            break;
                        case "banner":
                            viewProject.Banners.Add(ParseObject("banner"));
                            break;
                        case "datalog":
                            viewProject.DataLogs.Add(ParseObject("datalog"));
                            break;
                        case "shortcut":
                            viewProject.Shortcuts.Add(ParseShortcut());
                            break;
                        case "viewfolder":
                            viewProject.Folders.Add(ParseObject("viewfolder"));
                            break;
                        case "viewcontroller":
                            viewProject.Folders.Add(ParseObject("viewcontroller"));
                            break;
                        default:
                            ConsumeCurrent();
                            break;
                    }
                }
                else
                {
                    ConsumeCurrent();
                }
            }

            project.Project = viewProject;
            return project;
        }

        private void ParseViewProject(ViewProject viewProject)
        {
            ConsumeCurrent(); // 消费 'ViewProject'

            // 获取项目名称
            if (_currentToken.Type == TokenType.Identifier)
            {
                viewProject.Name = _currentToken.Value;
                ConsumeCurrent();
            }

            ExpectTokenType(TokenType.OpenBracket);
            ParseBlock((obj) =>
            {
                if (string.Equals(obj.Type, "HomeScreen", StringComparison.OrdinalIgnoreCase))
                {
                    if (obj.Properties.ContainsKey("value"))
                    {
                        viewProject.HomeScreen = obj.Properties["value"]?.ToString();
                    }
                }
            });
            ExpectTokenType(TokenType.CloseBracket);
        }

        private HmiObject ParseObject(string type)
        {
            var obj = new HmiObject { Type = type };
            ConsumeCurrent(); // 消费类型关键字

            // 获取对象名称（某些对象如Banner可能没有名称）
            if (_currentToken.Type == TokenType.Identifier)
            {
                obj.Name = _currentToken.Value;
                ConsumeCurrent();
            }

            // 解析参数
            if (_currentToken.Type == TokenType.OpenParen)
            {
                ConsumeCurrent();
                ParseParameters(obj);
                ExpectTokenType(TokenType.CloseParen);
            }

            // 解析主体
            if (_currentToken.Type == TokenType.OpenBracket)
            {
                ParseBlock((childObj) =>
                {
                    // 根据子对象类型分类存储
                    if (childObj.Type == "event" || childObj.Type == "ontouchpress" || 
                        childObj.Type == "ontouchrelease" || childObj.Type == "onstateenter" ||
                        childObj.Type == "onstateexit" || childObj.Type == "onkeypress" ||
                        childObj.Type == "onkeyrelease")
                    {
                        obj.Events.Add(childObj);
                    }
                    else if (childObj.Type.StartsWith("behavior", StringComparison.OrdinalIgnoreCase))
                    {
                        obj.Behaviors.Add(childObj);
                    }
                    else if (childObj.Type == "statetable" || childObj.Type == "statetabledefinition")
                    {
                        obj.StateTables.Add(childObj);
                    }
                    else if (childObj.Type == "securityroles")
                    {
                        obj.SecurityRoles = new Dictionary<string, string>();
                        foreach (var prop in childObj.Properties)
                        {
                            obj.SecurityRoles[prop.Key] = prop.Value?.ToString();
                        }
                    }
                    else if (childObj.Type == "userproperties")
                    {
                        obj.UserProperties.Add(childObj);
                    }
                    else if (childObj.Type == "datalogentry")
                    {
                        obj.Children.Add(childObj);
                    }
                    else if (childObj.Type == "state")
                    {
                        obj.StateTables.LastOrDefault()?.Children.Add(childObj);
                    }
                    else
                    {
                        obj.Children.Add(childObj);
                    }
                });
                ExpectTokenType(TokenType.CloseBracket);
            }

            return obj;
        }

        private HmiObject ParseShortcut()
        {
            var obj = new HmiObject { Type = "shortcut" };
            ConsumeCurrent(); // 消费 'shortcut'

            if (_currentToken.Type == TokenType.Identifier)
            {
                obj.Name = _currentToken.Value;
                ConsumeCurrent();
            }

            // 解析参数
            if (_currentToken.Type == TokenType.OpenParen)
            {
                ConsumeCurrent();
                ParseParameters(obj);
                ExpectTokenType(TokenType.CloseParen);
            }

            // 解析主体
            if (_currentToken.Type == TokenType.OpenBracket)
            {
                ParseBlock((childObj) => obj.Children.Add(childObj));
                ExpectTokenType(TokenType.CloseBracket);
            }

            return obj;
        }

        private void ParseBlock(Action<HmiObject> childHandler)
        {
            ConsumeCurrent(); // 消费 '{'

            while (_currentToken.Type != TokenType.CloseBracket && 
                   _currentToken.Type != TokenType.EndOfFile)
            {
                if (_currentToken.Type == TokenType.Identifier || 
                    _currentToken.Type == TokenType.Keyword)
                {
                    string name = _currentToken.Value;
                    ConsumeCurrent();

                    // 检查是否是属性赋值
                    if (_currentToken.Type == TokenType.Assign)
                    {
                        var child = new HmiObject { Type = "property", Name = name };
                        ParsePropertyValue(child);
                        childHandler(child);
                    }
                    // 检查是否是子对象定义
                    else if (_currentToken.Type == TokenType.Identifier || 
                             _currentToken.Type == TokenType.OpenBracket ||
                             _currentToken.Type == TokenType.OpenParen)
                    {
                        var childObj = ParseInlineObject(name);
                        if (childObj != null)
                        {
                            childHandler(childObj);
                        }
                    }
                }
                else if (_currentToken.Type == TokenType.Semicolon)
                {
                    // 跳过空的分号
                    ConsumeCurrent();
                }
                else if (_currentToken.Type == TokenType.CloseBracket)
                {
                    break;
                }
                else
                {
                    ConsumeCurrent();
                }
            }
        }

        private HmiObject ParseInlineObject(string typeName)
        {
            var obj = new HmiObject { Type = typeName };

            // 获取对象名称
            if (_currentToken.Type == TokenType.Identifier)
            {
                obj.Name = _currentToken.Value;
                ConsumeCurrent();
            }

            // 解析参数
            if (_currentToken.Type == TokenType.OpenParen)
            {
                ConsumeCurrent();
                ParseParameters(obj);
                ExpectTokenType(TokenType.CloseParen);
            }

            // 解析主体
            if (_currentToken.Type == TokenType.OpenBracket)
            {
                ParseBlock((childObj) =>
                {
                    if (childObj.Type == "event" || childObj.Name?.ToLower() == "ontouchpress" ||
                        childObj.Name?.ToLower() == "ontouchrelease")
                    {
                        obj.Events.Add(childObj);
                    }
                    else if (childObj.Type.StartsWith("behavior", StringComparison.OrdinalIgnoreCase))
                    {
                        obj.Behaviors.Add(childObj);
                    }
                    else if (childObj.Type == "statetable")
                    {
                        obj.StateTables.Add(childObj);
                    }
                    else
                    {
                        obj.Children.Add(childObj);
                    }
                });
                ExpectTokenType(TokenType.CloseBracket);
            }

            return obj;
        }

        private void ParseParameters(HmiObject obj)
        {
            while (_currentToken.Type != TokenType.CloseParen && 
                   _currentToken.Type != TokenType.EndOfFile)
            {
                if (_currentToken.Type == TokenType.Identifier)
                {
                    string paramName = _currentToken.Value;
                    ConsumeCurrent();

                    if (_currentToken.Type == TokenType.Assign)
                    {
                        ParsePropertyValue(obj);
                        // 修改名称为参数名
                        if (obj.Properties.ContainsKey("value"))
                        {
                            obj.Properties[paramName] = obj.Properties["value"];
                            obj.Properties.Remove("value");
                        }
                        if (obj.Properties.ContainsKey("bound"))
                        {
                            obj.Properties[paramName + "_bound"] = obj.Properties["bound"];
                            obj.Properties.Remove("bound");
                        }
                    }
                }
                else if (_currentToken.Type == TokenType.Comma)
                {
                    ConsumeCurrent();
                }
                else
                {
                    ConsumeCurrent();
                }
            }
        }

        private void ParsePropertyValue(HmiObject obj)
        {
            ConsumeCurrent(); // 消费 ':='

            // 解析值
            if (_currentToken.Type == TokenType.String)
            {
                obj.Properties["value"] = _currentToken.Value;
                ConsumeCurrent();
            }
            else if (_currentToken.Type == TokenType.Number)
            {
                obj.Properties["value"] = _currentToken.Value;
                ConsumeCurrent();
            }
            else if (_currentToken.Type == TokenType.Keyword)
            {
                string val = _currentToken.Value;
                ConsumeCurrent();
                
                // 处理枚举值，如 EnumUpdateRate._500_ms
                if (val == "EnumUpdateRate" || val.Contains("Enum"))
                {
                    if (_currentToken.Type == TokenType.Dot || 
                        _currentToken.Value.StartsWith("."))
                    {
                        val += _currentToken.Value;
                        ConsumeCurrent();
                        if (_currentToken.Type == TokenType.Identifier)
                        {
                            val += _currentToken.Value;
                            ConsumeCurrent();
                        }
                    }
                }
                obj.Properties["value"] = val;
            }
            else if (_currentToken.Type == TokenType.Identifier)
            {
                string val = _currentToken.Value;
                ConsumeCurrent();
                
                // 处理点分隔的枚举值
                if (_currentToken.Type == TokenType.Dot || 
                    (_currentToken.Value.StartsWith(".")))
                {
                    if (_currentToken.Value.StartsWith("."))
                    {
                        val += _currentToken.Value;
                        ConsumeCurrent();
                    }
                    else
                    {
                        ConsumeCurrent(); // 消费点号
                        val += ".";
                    }
                    
                    while (_currentToken.Type == TokenType.Identifier)
                    {
                        val += _currentToken.Value;
                        ConsumeCurrent();
                        if (_currentToken.Type == TokenType.Dot || 
                            (_currentToken.Value.StartsWith(".") && _currentToken.Value.Length > 1))
                        {
                            if (_currentToken.Value.StartsWith("."))
                            {
                                val += _currentToken.Value;
                                ConsumeCurrent();
                            }
                            else
                            {
                                ConsumeCurrent(); // 消费点号
                                val += ".";
                            }
                        }
                        else
                        {
                            break;
                        }
                    }
                }
                obj.Properties["value"] = val;
            }

            // 检查是否有绑定值
            if (_currentToken.Type == TokenType.Arrow)
            {
                ConsumeCurrent();
                if (_currentToken.Type == TokenType.String || 
                    _currentToken.Type == TokenType.Identifier)
                {
                    obj.Properties["bound"] = _currentToken.Value;
                    ConsumeCurrent();
                    
                    // 处理复杂绑定表达式（可能包含多个标识符和操作符）
                    while (_currentToken.Type == TokenType.Identifier || 
                           _currentToken.Type == TokenType.Dot ||
                           _currentToken.Type == TokenType.Operator)
                    {
                        if (_currentToken.Type == TokenType.Dot)
                        {
                            obj.Properties["bound"] += _currentToken.Value;
                            ConsumeCurrent();
                        }
                        else if (_currentToken.Type == TokenType.Identifier)
                        {
                            obj.Properties["bound"] += _currentToken.Value;
                            ConsumeCurrent();
                        }
                        else
                        {
                            break;
                        }
                    }
                }
            }

            // 消费分号
            if (_currentToken.Type == TokenType.Semicolon)
            {
                ConsumeCurrent();
            }
        }

        private void ExpectKeyword(string expected)
        {
            if (_currentToken.Type == TokenType.Keyword && 
                string.Equals(_currentToken.Value, expected, StringComparison.OrdinalIgnoreCase))
            {
                ConsumeCurrent();
            }
            else if (_currentToken.Type == TokenType.Identifier && 
                     string.Equals(_currentToken.Value, expected, StringComparison.OrdinalIgnoreCase))
            {
                ConsumeCurrent();
            }
        }

        private void ExpectTokenType(TokenType expected)
        {
            if (_currentToken.Type == expected)
            {
                ConsumeCurrent();
            }
        }

        private void ConsumeCurrent()
        {
            if (_currentToken.Type != TokenType.EndOfFile)
            {
                _currentToken = _tokenizer.NextToken();
            }
        }
    }
}