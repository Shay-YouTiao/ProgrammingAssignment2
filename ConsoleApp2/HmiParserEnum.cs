using System;
using System.Collections.Generic;

namespace HmiParser
{
    public enum PenStyle
    {
        NoPen = 0,
        DashDotLine = 4,
        CustomDashLine = 6,
        DotLine = 3,
        DashLine = 2,
        DashDotDotLine = 5,
        SolidLine = 1
    }

    public enum CapStyle
    {
        FlatCap = 0,
        RoundCap = 32,
        SquareCap = 16
    }

    public enum JoinStyle
    {
        MiterJoin = 0,
        BevelJoin = 64,
        RoundJoin = 128
    }

    public enum EnumAlignment
    {
        HLEFT_VTOP = 33,
        HLEFT_VCENTER = 129,
        HLEFT_VBOTTOM = 65,
        HCENTER_VTOP = 36,
        HCENTER_VCENTER = 132,
        HCENTER_VBOTTOM = 68,
        HRIGHT_VTOP = 34,
        HRIGHT_VCENTER = 130,
        HRIGHT_VBOTTOM = 66
    }

    public enum EnumUpdateRate
    {
        _500_ms = 0,
        _1000_ms = 1,
        _2000_ms = 2,
        _5000_ms = 3
    }

    public enum EnumElementAccessFamily
    {
        Inherit = 0,
        ReadOnly = 1,
        NoAccess = 2
    }

    public enum RoleAccess
    {
        FullAccess = 0,
        ReadOnly = 1,
        NoAccess = 2
    }

    public enum TokenType
    {
        Keyword,
        Identifier,
        String,
        Number,
        Assign,         // :=
        Bind,           // ->
        Semicolon,
        LBrace,
        RBrace,
        LParen,
        RParen,
        Comment,
        Namespace,
        Using,
        EOF
    }
}