package jw.flashcards;

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

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
        multiply("\u00D7")
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
        printImage(createLineImage());
    }
    
    public static BufferedImage createLineImage()
    {
        BufferedImage image = newImage();
        Graphics2D g = image.createGraphics();
        g.setColor(Color.black);
        g.setFont(Font.decode(null).deriveFont(18f));
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
    
    public static final int OPERATOR_SPACING = 20;
    public static final int MAX_NUMBER_WIDTH = 250 - OPERATOR_SPACING;
    public static final int MAX_NUMBER_HEIGHT = 150 - OPERATOR_SPACING;
    public static final int STARTING_SIZE = 150;
    
    public static BufferedImage createFlashcardFrontImage(int first,
            int second, Operation operation)
    {
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
        g.setFont(maxFontForSize(g, STARTING_SIZE, MAX_NUMBER_WIDTH,
                MAX_NUMBER_HEIGHT, operatorString, firstString, secondString));
        /*
         * Font size is set. Now we determine the position to start drawing each
         * sequence from.
         */
        int firstNumberX = getFromRight(g, firstString, totalWidth - 50);
        int firstNumberY = 25;
        int secondNumberX = getFromRight(g, secondString, totalWidth - 50);
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
            String symbol, String first, String second)
    {
        if (w == -1)
            w = Integer.MAX_VALUE;
        if (h == -1)
            h = Integer.MAX_VALUE;
        while (size > 0)
        {
            Font font = Font.decode(null).deriveFont(size * 1f);
            FontMetrics metrics = g.getFontMetrics(font);
            int symbolW = metrics.stringWidth(symbol);
            int firstW = metrics.stringWidth(first);
            int secondW = metrics.stringWidth(second);
            int rw = Math.max(firstW, secondW) + symbolW;
            int rh = metrics.getAscent();
            if (w <= rw && h <= rh)
                return font;
            size -= 1;
        }
        if (size <= 0)
            throw new IllegalArgumentException(
                    "The font couldn't fit into that box.");
        return Font.decode(null).deriveFont(size * 1f);
    }
    
    public static BufferedImage newImage()
    {
        BufferedImage image = new BufferedImage(300, 500,
                BufferedImage.TYPE_INT_ARGB);
        Graphics g = image.getGraphics();
        g.setColor(Color.WHITE);
        g.fillRect(0, 0, 300, 500);
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
    
}
