using System;

namespace HmiParser
{
    /// <summary>
    /// 语法错误异常
    /// </summary>
    public class SyntaxErrorException : Exception
    {
        public SyntaxErrorException(string message) : base(message)
        {
        }

        public SyntaxErrorException(string message, Exception innerException) 
            : base(message, innerException)
        {
        }
    }
}