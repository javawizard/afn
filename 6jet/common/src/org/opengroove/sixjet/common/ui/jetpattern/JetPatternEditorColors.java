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
    
    static final Color markNormalStart = ;
}
