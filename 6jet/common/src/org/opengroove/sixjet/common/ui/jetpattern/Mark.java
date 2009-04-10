package org.opengroove.sixjet.common.ui.jetpattern;

import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Paint;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.border.Border;

/**
 * A mark on a jet pattern editor.
 * 
 * @author Alexander Boyd
 * 
 */
public class Mark extends JComponent
{
    private JetPatternEditor editor;
    
    private static final Border markBorder = BorderFactory.createLineBorder(JetPatternEditorColors.markBorder, 1)
    
    Mark(JetPatternEditor editor)
    {
        this.editor = editor;
        setBorder();
    }
    
    protected void paintComponent(Graphics g)
    {
        // TODO Auto-generated method stub
        super.paintComponent(g);
    }
    
    Paint createNormalPaint()
    {
        return shadedVertical(JetPatternEditorColors.markNormalStart,
            JetPatternEditorColors.markNormalEnd);
    }
    
    private Paint shadedVertical(Color top, Color bottom)
    {
        return new GradientPaint(0, 0, top, 0, getHeight(), bottom);
    }
    
    Paint createHoveredPaint()
    {
        return shadedVertical(JetPatternEditorColors.markNormalStart.brighter(),
            JetPatternEditorColors.markNormalEnd.brighter());
    }
    
    Paint createSelectedPaint()
    {
        return shadedVertical(JetPatternEditorColors.markSelectedStart,
            JetPatternEditorColors.markSelectedEnd);
    }
    
    Paint createSelectedHoveredPaint()
    {
        return shadedVertical(JetPatternEditorColors.markSelectedStart.brighter(),
            JetPatternEditorColors.markSelectedEnd.brighter());
    }
    
}
