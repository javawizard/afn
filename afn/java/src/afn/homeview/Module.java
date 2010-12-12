package afn.homeview;

import java.awt.Button;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Label;
import java.awt.Panel;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;

public class Module extends Panel
{
    public static class ButtonListener implements ActionListener
    {
        private String address;
        private String state;
        
        public ButtonListener(String address, String state)
        {
            super();
            this.address = address;
            this.state = state;
        }
        
        @Override
        public void actionPerformed(ActionEvent e)
        {
            HomeView.bus.getInterface("home").getFunction(state).send(address);
        }
    }
    
    private String address;
    private String name;
    private Label addressLabel;
    private Label nameLabel;
    private Button onButton;
    private Button offButton;
    private static final Insets insets = new Insets(5, 5, 5, 5);
    
    public Module(String address, String name)
    {
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        this.address = address;
        this.name = name;
        this.addressLabel = new Label(address.toUpperCase());
        this.nameLabel = new Label(name);
        this.onButton = new Button("on");
        this.offButton = new Button("off");
        onButton.addActionListener(new ButtonListener(address, "on"));
        offButton.addActionListener(new ButtonListener(address, "off"));
        addressLabel.setAlignment(Label.CENTER);
        nameLabel.setAlignment(Label.CENTER);
        nameLabel.setFont(Font.decode(null).deriveFont(10f));
        Panel buttonPanel = new Panel(new GridLayout(1, 2, 1, 1));
        buttonPanel.add(onButton);
        buttonPanel.add(offButton);
        add(addressLabel);
        add(nameLabel);
        add(buttonPanel);
    }
    
    public Insets getInsets()
    {
        return insets;
    }
    
    public void paint(Graphics g)
    {
        super.paint(g);
        g.setColor(Color.BLACK);
        g.drawRect(3, 3, getWidth() - 7, getHeight() - 7);
    }
    
    public Dimension getPreferredSize()
    {
        return new Dimension(95, super.getPreferredSize().height);
    }
    
    public void setName(String name)
    {
        if (!name.equals(this.name))
        {
            this.name = name;
            this.nameLabel.setText(name);
        }
    }
    
    public String getAddress()
    {
        return address;
    }
    
}