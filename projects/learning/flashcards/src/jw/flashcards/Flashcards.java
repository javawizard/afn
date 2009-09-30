package jw.flashcards;

import java.awt.Color;
import java.awt.Font;
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
