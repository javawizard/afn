package org.opengroove.sixjet.common.ui;

import info.clearthought.layout.TableLayout;

import java.awt.Color;

import javax.swing.JComponent;
import javax.swing.JPanel;

import org.opengroove.sixjet.common.format.d.DescriptorFile;
import org.opengroove.sixjet.common.ui.jetpattern.JetPatternEditorColors;

/**
 * A component that shows the jets in a passed-in descriptor file object
 * on-screen. Each of these jets can be clicked. Listeners can be added to be
 * notified when the mouse goes down over a jet and when the mouse goes up over
 * a jet. If the mouse goes down over a jet and then moves out, the jet will
 * still remain "down", but the jet will be "up" when the mouse is released,
 * even if it has moved elsewhere.<br/>
 * <br/>
 * 
 * The jet positions are centered on the component, so that if it is resized,
 * the jets will remain in the center.<br/>
 * <br/>
 * 
 * The application that uses this component can set a particular jet's state (on
 * or off). The jet will render differently depending on its state. This
 * component does not automatically change a jet's state in response to mouse
 * clicks; it's up to the application to do that.
 * 
 * @author Alexander Boyd
 * 
 */
public class JetDisplayComponent extends JComponent
{
    private static final Color groundColor = JetPatternEditorColors.hexColor("17b130");
    private static final Color waterColor = JetPatternEditorColors.hexColor("00d3ec");
    private static final Color waterOffColor =
        JetPatternEditorColors.hexColor("a0a0a0b2");
    private static final Color lineColor = JetPatternEditorColors.hexColor("000000");
    private static final Color lineOffColor = JetPatternEditorColors.hexColor("000000");
    
    private static class JetState
    {
        public boolean state;
    }
    
    private static class UIJet extends JComponent
    {
        
    }
    
    private DescriptorFile descriptor;
    private JetState[] jets;
    
    public JetDisplayComponent(DescriptorFile descriptor)
    {
        jets = new JetState[descriptor.getJets().size()];
        setLayout(new TableLayout(
            new double[][] {
                new double[] { TableLayout.FILL, TableLayout.PREFERRED,
                    TableLayout.FILL },
                new double[] { TableLayout.FILL, TableLayout.PREFERRED,
                    TableLayout.FILL } }));
        JPanel panel = new JPanel();
        panel.setLayout(null);
        panel.setPreferredSize(new Dimension(descriptor.getWidth(), descriptor
            .getHeight()));
        for (int i = 0; i < descriptor.getJets().size(); i++)
        {
            
        }
    }
    
}
