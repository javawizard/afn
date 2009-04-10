package org.opengroove.sixjet.common.ui.jetpattern;

import java.awt.Color;

public class JetPatternEditorColors
{
    private static Color hexColor(String hex)
    {
        String rs = hex.substring(0, 2);
        String gs = hex.substring(2, 4);
        String bs = hex.substring(4, 6);
        return new Color(Integer.parseInt(rs, 16), Integer.parseInt(gs, 16), Integer
            .parseInt(bs, 16));
    }
    
    static final Color markNormalStart = hexColor("b9d1e3");
    static final Color markNormalEnd = hexColor("6195c1");
    static final Color markSelectedStart = hexColor("5b91be");
    static final Color markSelectedEnd = hexColor("5b91be");
    static final Color markBorder = hexColor("3e8ebb");
}
