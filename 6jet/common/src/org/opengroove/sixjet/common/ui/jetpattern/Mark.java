package org.opengroove.sixjet.common.ui.jetpattern;

import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Paint;

import javax.swing.JComponent;

/**
 * A mark on a jet pattern editor.
 * 
 * @author Alexander Boyd
 * 
 */
public class Mark extends JComponent
{
    private JetPatternEditor editor;
    
    Mark(JetPatternEditor editor)
    {
        this.editor = editor;
    }
    
    protected void paintComponent(Graphics g)
    {
        // TODO Auto-generated method stub
        super.paintComponent(g);
    }
    
    Paint createNormalPaint()
    {
        return new GradientPaint()
    }
    
    Paint createHoveredPaint()
    {
        
    }
    
    Paint createSelectedPaint()
    {
        
    }
    
}
