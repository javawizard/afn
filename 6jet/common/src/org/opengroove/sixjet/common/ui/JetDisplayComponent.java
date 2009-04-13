package org.opengroove.sixjet.common.ui;

import info.clearthought.layout.TableLayout;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;

import javax.swing.JComponent;
import javax.swing.JPanel;

import org.opengroove.sixjet.common.format.d.DescriptorFile;
import org.opengroove.sixjet.common.format.d.DescriptorFile.DescriptorFileJet;
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
    
    private class JetState
    {
        public boolean state;
    }
    
    private class UIJet extends JComponent implements MouseListener
    {
        private int number;
        
        private JetState state;
        
        public UIJet(int index, int number, int x, int y, JetState state)
        {
            this.number = number;
            this.state = state;
            setSize(24, 24);
            setLocation(x - 12, y - 12);
            setOpaque(false);
            addMouseListener(this);
        }
        
        public void paintComponent(Graphics g)
        {
            Graphics2D g2 = (Graphics2D) g;
            if (state.state)
                g2.setColor(waterColor);
            else
                g2.setColor(waterOffColor);
            g2.fillOval(0, 0, 24, 24);
            if (state.state)
                g2.setColor(lineColor);
            else
                g2.setColor(lineOffColor);
            g2.drawOval(0, 0, 24, 24);
            /*
             * TODO: possibly add stuff to color a jet lighter when it is being
             * hovered. Maybe allow the application using the component to
             * determine whether hovering is shown, to indicate when a jet
             * display component is read-only (such as during music playback)
             */
        }
        
        public void mouseClicked(MouseEvent e)
        {
        }
        
        public void mouseEntered(MouseEvent e)
        {
        }
        
        public void mouseExited(MouseEvent e)
        {
        }
        
        public void mousePressed(MouseEvent e)
        {
            notifyListeners(false, number, true);
        }
        
        public void mouseReleased(MouseEvent e)
        {
            notifyListeners(true, number, new Rectangle(getSize()).contains(e.getX(), e
                .getY()));
        }
        
    }
    
    private DescriptorFile descriptor;
    private JetState[] jets;
    private ArrayList<JetDisplayListener> listeners =
        new ArrayList<JetDisplayListener>();
    
    public JetDisplayComponent(DescriptorFile descriptor)
    {
        this.descriptor = descriptor;
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
        panel.setOpaque(true);
        panel.setBackground(groundColor);
        setBackground(groundColor);
        setOpaque(true);
        add(panel, "1,1");
        setPreferredSize(panel.getPreferredSize());
        setMinimumSize(getPreferredSize());
        for (int i = 0; i < descriptor.getJets().size(); i++)
        {
            jets[i] = new JetState();
            DescriptorFileJet fileJet = descriptor.getJets().get(i);
            UIJet jet = new UIJet(i, fileJet.number, fileJet.x, fileJet.y, jets[i]);
            panel.add(jet);
        }
    }
    
    private void notifyListeners(boolean up, int number, boolean in)
    {
        for (JetDisplayListener l : new ArrayList<JetDisplayListener>(listeners))
        {
            if (up)
                l.jetUp(number, in);
            else
                l.jetDown(number);
        }
    }
    
    /**
     * Sets the state of a jet.
     * 
     * @param number
     *            The number, as specified in the descriptor, of the jet
     * @param state
     *            The new state of the jet, true for on, false for off
     */
    public void setState(int number, boolean state)
    {
        
    }
    
    public void addJetListener(JetDisplayListener l)
    {
        listeners.add(l);
    }
    
    public void removeJetListener(JetDisplayListener l)
    {
        listeners.remove(l);
    }
}
