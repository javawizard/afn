package org.opengroove.sixjet.landfill;

import java.awt.EventQueue;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.jdesktop.beansbinding.AutoBinding;
import org.jdesktop.beansbinding.BeanProperty;
import org.jdesktop.beansbinding.BindingGroup;
import org.jdesktop.beansbinding.Bindings;

public class TestBinding1 extends JFrame {
    
    private BindingGroup m_bindingGroup;
    private JPanel m_contentPane;
    private org.opengroove.sixjet.landfill.TestBean1 testBean1 = new org.opengroove.sixjet.landfill.TestBean1();
    private JCheckBox numberOneJCheckBox;
    private JCheckBox numberThreeJCheckBox;
    private JCheckBox numberTwoJCheckBox;
    private JTextField titleJTextField;
    
    /**
     * Launch the application.
     */
    public static void main(String[] args) {
        EventQueue.invokeLater(new Runnable() {
            public void run() {
                try {
                    TestBinding1 frame = new TestBinding1();
                    frame.setVisible(true);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        });
    }
    
    /**
     * Create the frame.
     */
    public TestBinding1() {
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setBounds(100, 100, 450, 300);
        m_contentPane = new JPanel();
        setContentPane(m_contentPane);
        //
        GridBagLayout gridBagLayout = new GridBagLayout();
        gridBagLayout.columnWidths = new int[] { 0, 0, 0 };
        gridBagLayout.rowHeights = new int[] { 0, 0, 0, 0, 0 };
        gridBagLayout.columnWeights = new double[] { 0.0, 1.0, 1.0E-4 };
        gridBagLayout.rowWeights = new double[] { 0.0, 0.0, 0.0, 0.0, 1.0E-4 };
        m_contentPane.setLayout(gridBagLayout);
        
        JLabel numberOneLabel = new JLabel("NumberOne:");
        GridBagConstraints labelGbc_0 = new GridBagConstraints();
        labelGbc_0.insets = new Insets(5, 5, 5, 5);
        labelGbc_0.gridx = 0;
        labelGbc_0.gridy = 0;
        m_contentPane.add(numberOneLabel, labelGbc_0);
        
        numberOneJCheckBox = new JCheckBox();
        GridBagConstraints componentGbc_0 = new GridBagConstraints();
        componentGbc_0.insets = new Insets(5, 0, 5, 5);
        componentGbc_0.fill = GridBagConstraints.HORIZONTAL;
        componentGbc_0.gridx = 1;
        componentGbc_0.gridy = 0;
        m_contentPane.add(numberOneJCheckBox, componentGbc_0);
        
        JLabel numberThreeLabel = new JLabel("NumberThree:");
        GridBagConstraints labelGbc_1 = new GridBagConstraints();
        labelGbc_1.insets = new Insets(5, 5, 5, 5);
        labelGbc_1.gridx = 0;
        labelGbc_1.gridy = 1;
        m_contentPane.add(numberThreeLabel, labelGbc_1);
        
        numberThreeJCheckBox = new JCheckBox();
        GridBagConstraints componentGbc_1 = new GridBagConstraints();
        componentGbc_1.insets = new Insets(5, 0, 5, 5);
        componentGbc_1.fill = GridBagConstraints.HORIZONTAL;
        componentGbc_1.gridx = 1;
        componentGbc_1.gridy = 1;
        m_contentPane.add(numberThreeJCheckBox, componentGbc_1);
        
        JLabel numberTwoLabel = new JLabel("NumberTwo:");
        GridBagConstraints labelGbc_2 = new GridBagConstraints();
        labelGbc_2.insets = new Insets(5, 5, 5, 5);
        labelGbc_2.gridx = 0;
        labelGbc_2.gridy = 2;
        m_contentPane.add(numberTwoLabel, labelGbc_2);
        
        numberTwoJCheckBox = new JCheckBox();
        GridBagConstraints componentGbc_2 = new GridBagConstraints();
        componentGbc_2.insets = new Insets(5, 0, 5, 5);
        componentGbc_2.fill = GridBagConstraints.HORIZONTAL;
        componentGbc_2.gridx = 1;
        componentGbc_2.gridy = 2;
        m_contentPane.add(numberTwoJCheckBox, componentGbc_2);
        
        JLabel titleLabel = new JLabel("Title:");
        GridBagConstraints labelGbc_3 = new GridBagConstraints();
        labelGbc_3.insets = new Insets(5, 5, 5, 5);
        labelGbc_3.gridx = 0;
        labelGbc_3.gridy = 3;
        m_contentPane.add(titleLabel, labelGbc_3);
        
        titleJTextField = new JTextField();
        GridBagConstraints componentGbc_3 = new GridBagConstraints();
        componentGbc_3.insets = new Insets(5, 0, 5, 5);
        componentGbc_3.fill = GridBagConstraints.HORIZONTAL;
        componentGbc_3.gridx = 1;
        componentGbc_3.gridy = 3;
        m_contentPane.add(titleJTextField, componentGbc_3);
        
        if (testBean1 != null) {
            m_bindingGroup = initDataBindings();
        }
    }
    
    protected BindingGroup initDataBindings() {
        BeanProperty<org.opengroove.sixjet.landfill.TestBean1, java.lang.Boolean> numberOneProperty = BeanProperty
                .create("numberOne");
        BeanProperty<javax.swing.JCheckBox, java.lang.Boolean> selectedProperty = BeanProperty
                .create("selected");
        AutoBinding<org.opengroove.sixjet.landfill.TestBean1, java.lang.Boolean, javax.swing.JCheckBox, java.lang.Boolean> autoBinding = Bindings
                .createAutoBinding(AutoBinding.UpdateStrategy.READ, testBean1,
                        numberOneProperty, numberOneJCheckBox, selectedProperty);
        autoBinding.bind();
        //
        BeanProperty<org.opengroove.sixjet.landfill.TestBean1, java.lang.Boolean> numberThreeProperty = BeanProperty
                .create("numberThree");
        BeanProperty<javax.swing.JCheckBox, java.lang.Boolean> selectedProperty_1 = BeanProperty
                .create("selected");
        AutoBinding<org.opengroove.sixjet.landfill.TestBean1, java.lang.Boolean, javax.swing.JCheckBox, java.lang.Boolean> autoBinding_1 = Bindings
                .createAutoBinding(AutoBinding.UpdateStrategy.READ, testBean1,
                        numberThreeProperty, numberThreeJCheckBox,
                        selectedProperty_1);
        autoBinding_1.bind();
        //
        BeanProperty<org.opengroove.sixjet.landfill.TestBean1, java.lang.Boolean> numberTwoProperty = BeanProperty
                .create("numberTwo");
        BeanProperty<javax.swing.JCheckBox, java.lang.Boolean> selectedProperty_2 = BeanProperty
                .create("selected");
        AutoBinding<org.opengroove.sixjet.landfill.TestBean1, java.lang.Boolean, javax.swing.JCheckBox, java.lang.Boolean> autoBinding_2 = Bindings
                .createAutoBinding(AutoBinding.UpdateStrategy.READ, testBean1,
                        numberTwoProperty, numberTwoJCheckBox,
                        selectedProperty_2);
        autoBinding_2.bind();
        //
        BeanProperty<org.opengroove.sixjet.landfill.TestBean1, java.lang.String> titleProperty = BeanProperty
                .create("title");
        BeanProperty<javax.swing.JTextField, java.lang.String> textProperty = BeanProperty
                .create("text");
        AutoBinding<org.opengroove.sixjet.landfill.TestBean1, java.lang.String, javax.swing.JTextField, java.lang.String> autoBinding_3 = Bindings
                .createAutoBinding(AutoBinding.UpdateStrategy.READ_WRITE,
                        testBean1, titleProperty, titleJTextField, textProperty);
        autoBinding_3.bind();
        //
        BindingGroup bindingGroup = new BindingGroup();
        bindingGroup.addBinding(autoBinding);
        bindingGroup.addBinding(autoBinding_1);
        bindingGroup.addBinding(autoBinding_2);
        bindingGroup.addBinding(autoBinding_3);
        //
        return bindingGroup;
    }
    
    public org.opengroove.sixjet.landfill.TestBean1 getTestBean1() {
        return testBean1;
    }
    
    public void setTestBean1(
            org.opengroove.sixjet.landfill.TestBean1 newTestBean1) {
        setTestBean1(newTestBean1, true);
    }
    
    public void setTestBean1(
            org.opengroove.sixjet.landfill.TestBean1 newTestBean1,
            boolean update) {
        testBean1 = newTestBean1;
        if (update) {
            if (m_bindingGroup != null) {
                m_bindingGroup.unbind();
                m_bindingGroup = null;
            }
            if (testBean1 != null) {
                m_bindingGroup = initDataBindings();
            }
        }
    }
    
}
