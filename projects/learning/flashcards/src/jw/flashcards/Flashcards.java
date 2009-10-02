package jw.flashcards;

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;

import javax.imageio.ImageIO;
import javax.print.DocFlavor;
import javax.print.DocPrintJob;
import javax.print.PrintException;
import javax.print.PrintService;
import javax.print.PrintServiceLookup;
import javax.print.SimpleDoc;
import javax.print.attribute.DocAttributeSet;
import javax.print.attribute.HashDocAttributeSet;
import javax.print.attribute.HashPrintRequestAttributeSet;
import javax.print.attribute.Size2DSyntax;
import javax.print.attribute.standard.MediaPrintableArea;
import javax.print.attribute.standard.MediaSize;
import javax.print.attribute.standard.MediaSizeName;
import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JProgressBar;
import javax.swing.border.LineBorder;

public class Flashcards
{
    public enum Operation
    {
        add("+")
        {
            @Override
            public String compute(int first, int second)
            {
                return "" + (first + second);
            }
        },
        subtract("-")
        {
            @Override
            public String compute(int first, int second)
            {
                return "" + (first - second);
            }
        },
        multiply("\u00D7")// u00D7
        {
            @Override
            public String compute(int first, int second)
            {
                return "" + (first * second);
            }
        };
        private String symbol;
        
        private Operation(String symbol)
        {
            this.symbol = symbol;
        }
        
        /**
         * Computes the answer to a problem as it should appear on the back of a
         * card. For example, multiply.compute(3,4) returns "12".
         * 
         * @param first
         *            The first number
         * @param second
         *            The second number
         * @return The answer
         */
        public abstract String compute(int first, int second);
        
        public String toString()
        {
            return symbol;
        }
    }
    
    public static final int INCHES = Size2DSyntax.INCH;
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Exception
    {
        final ChoiceFrame frame = new ChoiceFrame();
        frame.setTitle("Flashcards");
        frame.setLocationRelativeTo(null);
        frame.show();
        frame.getGoButton().addActionListener(new ActionListener()
        {
            
            @Override
            public void actionPerformed(ActionEvent e)
            {
                frame.getGoButton().setEnabled(false);
                new Thread()
                {
                    public void run()
                    {
                        doStartPrint(frame);
                    }
                }.start();
            }
        });
    }
    
    protected static void doStartPrint(ChoiceFrame frame)
    {
        frame.getFirstStart().setEnabled(false);
        frame.getFirstEnd().setEnabled(false);
        frame.getSecondStart().setEnabled(false);
        frame.getSecondEnd().setEnabled(false);
        frame.getOperationField().setEnabled(false);
        frame.getSideField().setEnabled(false);
        frame.getGoButton().setEnabled(false);
        JDialog dialog = null;
        try
        {
            int firstStart = Integer.parseInt(frame.getFirstStart().getText());
            int firstEnd = Integer.parseInt(frame.getFirstEnd().getText());
            int secondStart = Integer
                    .parseInt(frame.getSecondStart().getText());
            int secondEnd = Integer.parseInt(frame.getSecondEnd().getText());
            Operation operation = Operation.valueOf(""
                    + frame.getOperationField().getSelectedItem());
            boolean front = frame.getSideField().getSelectedIndex() == 0;
            boolean back = !front;
            ArrayList<int[]> items = new ArrayList<int[]>();
            for (int first = firstStart; first <= firstEnd; first++)
            {
                for (int second = secondStart; second <= secondEnd; second++)
                {
                    items.add(new int[]
                    {
                            first, second
                    });
                }
            }
            if (back)
                Collections.reverse(items);
            if (JOptionPane.showConfirmDialog(frame,
                    "Are you sure you want to print the "
                            + (front ? "front" : "back") + " side for "
                            + items.size() + " cards?", null,
                    JOptionPane.YES_NO_OPTION) != JOptionPane.YES_OPTION)
                return;
            dialog = new JDialog(frame, true);
            final JDialog fDialog = dialog;
            dialog.setTitle("Progress");
            JProgressBar progress = new JProgressBar();
            progress.setMaximum(items.size());
            progress.setMinimum(0);
            progress.setValue(0);
            progress.setStringPainted(true);
            progress.setString("Initializing...");
            dialog.getContentPane().add(progress);
            dialog.pack();
            dialog.setLocationRelativeTo(null);
            new Thread()
            {
                public void run()
                {
                    fDialog.show();
                }
            }.start();
            int index = 0;
            for (int[] item : items)
            {
                index++;
                System.out.println("Item " + index + " of " + items.size());
                progress.setValue(index);
                progress.setString("Printing card " + index + " of "
                        + items.size());
                int first = item[0];
                int second = item[1];
                System.gc();
                BufferedImage image = front ? createFlashcardFrontImage(first,
                        second, operation) : createFlashcardBackImage(first,
                        second, operation);
                printImage(image);
            }
            JOptionPane.showMessageDialog(frame, "Successfully printed.");
        }
        catch (Exception e)
        {
            e.printStackTrace();
            JOptionPane.showMessageDialog(frame, "An error occured: "
                    + e.getClass().getName() + ": " + e.getMessage());
        }
        finally
        {
            frame.getFirstStart().setEnabled(true);
            frame.getFirstEnd().setEnabled(true);
            frame.getSecondStart().setEnabled(true);
            frame.getSecondEnd().setEnabled(true);
            frame.getOperationField().setEnabled(true);
            frame.getSideField().setEnabled(true);
            frame.getGoButton().setEnabled(true);
            if (dialog != null)
                dialog.dispose();
        }
    }
    
    public static BufferedImage createLineImage()
    {
        BufferedImage image = newImage();
        Graphics2D g = image.createGraphics();
        g.setColor(Color.black);
        g.setFont(DEFAULT_FONT.deriveFont(18f));
        for (int i = 1; i < 300; i += 49)
        {
            g.drawLine(i, 0, i, 500);
        }
        for (int i = 1; i < 500; i += 49)
        {
            g.drawLine(0, i, 300, i);
        }
        return image;
    }
    
    public static final int IMAGE_WIDTH = 1200;
    public static final int IMAGE_HEIGHT = (IMAGE_WIDTH / 3) * 5;
    public static final int GM = IMAGE_WIDTH / 6;
    public static final int GMH = GM / 2;
    public static final int OPERATOR_SPACING = 0;
    public static final int FROM_SIDE = gm(1);
    public static final int MAX_NUMBER_WIDTH = (IMAGE_WIDTH - FROM_SIDE)
            - OPERATOR_SPACING;
    public static final int MAX_NUMBER_HEIGHT = gm(3.5);
    public static final int STARTING_SIZE = gm(4);
    
    public static final double SYMBOL_MULTIPLIER = 0.75d;
    private static final Font DEFAULT_FONT = Font.decode(null);
    public static final int LINE_START = gm(0.5);
    public static final int LINE_END = gm(6) - LINE_START;
    public static final int ANSWER_HEIGHT = gm(0.5);
    public static final int ANSWER_WIDTH = gm(1);
    public static final int ANSWER_X = gm(0.5);
    public static final int ANSWER_Y = gm(0.5);
    
    public static BufferedImage createFlashcardFrontImage(int first,
            int second, Operation operation)
    {
        double symbolMultiplier = SYMBOL_MULTIPLIER;
        BufferedImage image = newImage();
        Graphics2D g = image.createGraphics();
        int totalWidth = image.getWidth();
        int totalHeight = image.getHeight();
        g.setColor(Color.black);
        /*
         * This is the first number.
         */
        String firstString = "" + first;
        /*
         * This is the second number.
         */
        String secondString = "" + second;
        String operatorString = "" + operation;
        /*
         * We need to set the font size to allow the numbers to correctly fit
         */
        Font normalFont = maxFontForSize(g, STARTING_SIZE, MAX_NUMBER_WIDTH,
                MAX_NUMBER_HEIGHT, operatorString, firstString, secondString,
                symbolMultiplier);
        Font symbolFont = normalFont
                .deriveFont((float) (normalFont.getSize2D() * symbolMultiplier));
        g.setFont(normalFont);
        /*
         * Font size is set. Now we determine the position to start drawing each
         * sequence from.
         */
        int firstNumberX = getFromRight(g, firstString, totalWidth - FROM_SIDE);
        int firstNumberY = gm(3.5);
        int secondNumberX = getFromRight(g, secondString, totalWidth
                - FROM_SIDE);
        int secondNumberY = gm(7);
        g.drawString(firstString, firstNumberX, firstNumberY);
        g.drawString(secondString, secondNumberX, secondNumberY);
        g.setFont(symbolFont);
        int operatorX = getFromRight(g, operatorString, Math.min(firstNumberX,
                secondNumberX)
                - OPERATOR_SPACING);
        int operatorY = secondNumberY;
        g.drawString(operatorString, operatorX, operatorY);
        int lineY = gm(7.5);
        for (int i = 0; i < gm(0.1666); i++)
        {
            g.drawLine(LINE_START, lineY + i, LINE_END, lineY + i);
        }
        // Now we'll draw the box to hide the answer.
        g.fillRect((gm(6) - ANSWER_X) - ANSWER_WIDTH, (totalHeight - ANSWER_Y)
                - ANSWER_HEIGHT, ANSWER_WIDTH, ANSWER_HEIGHT);
        return image;
    }
    
    public static int gm(double multiplier)
    {
        return (int) ((GM * 1d) * multiplier);
    }
    
    public static BufferedImage createFlashcardBackImage(int first, int second,
            Operation operation)
    {
        BufferedImage image = newImage();
        Graphics2D g = image.createGraphics();
        g.setColor(Color.black);
        g.setFont(DEFAULT_FONT.deriveFont(ANSWER_HEIGHT * 1f));
        g.drawString(operation.compute(first, second), ANSWER_X, image
                .getHeight()
                - ANSWER_Y);
        String problemString = "" + first + operation.toString() + second;
        g.drawString(problemString, getFromRight(g, problemString, IMAGE_WIDTH
                - ANSWER_X), ANSWER_Y + ANSWER_HEIGHT);
        return image;
    }
    
    public static int getFromRight(Graphics g, String s, int position)
    {
        int width = g.getFontMetrics().stringWidth(s);
        return position - width;
    }
    
    /**
     * Returns the default font, but with a font size that is not bigger than
     * <tt>size</tt>, and would result in <tt>s</tt>, being drawn with that
     * font, being able to be contained within a box of width <tt>w</tt> and
     * height <tt>h</tt>.
     * 
     * @param g
     *            The graphics to test font sizes on
     * @param size
     *            The maximum size for the font to return
     * @param w
     *            The width that <tt>s</tt> must fit within
     * @param h
     *            The height that <tt>s</tt> must fit within
     * @param s
     *            The string to test against <tt>w</tt> and <tt>h</tt>
     * @return a font
     */
    public static Font maxFontForSize(Graphics g, int size, int w, int h,
            String symbol, String first, String second, double symbolMultiplier)
    {
        if (w == -1)
            w = Integer.MAX_VALUE;
        if (h == -1)
            h = Integer.MAX_VALUE;
        while (size > 0)
        {
            Font font = DEFAULT_FONT.deriveFont(size * 1f);
            FontMetrics metrics = g.getFontMetrics(font);
            int symbolW = metrics.stringWidth(symbol);
            symbolW = (int) ((symbolW * 1d) * symbolMultiplier);
            int firstW = metrics.stringWidth(first);
            int secondW = metrics.stringWidth(second);
            int rw = Math.max(firstW, secondW) + symbolW;
            int rh = metrics.getAscent();
            if (w >= rw && h >= rh)
            {
                System.out.println("Corrected ascent: " + metrics.getAscent()
                        + ", requested w: " + w + ", real w: " + rw
                        + ", first w: " + firstW + ", secondW: " + secondW
                        + ", rh: " + rh + ", h: " + h);
                return font;
            }
            size -= 1;
        }
        if (size <= 0)
            throw new IllegalArgumentException(
                    "The font couldn't fit into that box.");
        return DEFAULT_FONT.deriveFont(size * 1f);
    }
    
    public static BufferedImage newImage()
    {
        BufferedImage image = new BufferedImage(IMAGE_WIDTH, IMAGE_HEIGHT,
                BufferedImage.TYPE_INT_ARGB);
        Graphics g = image.getGraphics();
        g.setColor(Color.WHITE);
        g.fillRect(0, 0, IMAGE_WIDTH, IMAGE_HEIGHT);
        return image;
    }
    
    public static byte[] imageToData(BufferedImage image) throws IOException
    {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        ImageIO.write(image, "PNG", out);
        return out.toByteArray();
    }
    
    public static void printImage(BufferedImage image) throws IOException,
            PrintException
    {
        byte[] data = imageToData(image);
        PrintService service = PrintServiceLookup.lookupDefaultPrintService();
        DocPrintJob job = service.createPrintJob();
        HashPrintRequestAttributeSet attributes = new HashPrintRequestAttributeSet();
        attributes.add(MediaSizeName.NA_LETTER);
        MediaSize mediaSize = MediaSize
                .getMediaSizeForName(MediaSizeName.NA_LETTER);
        float pageWidth = mediaSize.getX(INCHES);
        float targetWidth = 3;
        float pageMiddle = pageWidth / 2;
        float targetMiddle = targetWidth / 2;
        attributes.add(new MediaPrintableArea((pageMiddle - targetMiddle)
                + (0.1f + 0.04f), 0.17f, 3 - (0.1f * 2), 5 - (0.17f * 2),
                INCHES));
        DocAttributeSet docAttributes = new HashDocAttributeSet();
        SimpleDoc doc = new SimpleDoc(data, DocFlavor.BYTE_ARRAY.PNG,
                docAttributes);
        job.print(doc, attributes);
    }
    
    public static void showImageInFrame(BufferedImage image)
    {
        JFrame f = new JFrame("Flashcards");
        f.setDefaultCloseOperation(f.DISPOSE_ON_CLOSE);
        JLabel l = new JLabel();
        l.setBorder(new LineBorder(Color.LIGHT_GRAY));
        l.setIcon(new ImageIcon(image));
        f.getContentPane().add(l);
        f.pack();
        f.setLocationRelativeTo(null);
        f.show();
    }
    
}
