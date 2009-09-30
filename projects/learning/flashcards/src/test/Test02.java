package test;

import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.util.Arrays;

import javax.imageio.ImageIO;
import javax.print.DocFlavor;
import javax.print.DocPrintJob;
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

public class Test02
{
    public static final int INCHES = Size2DSyntax.INCH;
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Throwable
    {
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
        attributes.add(new MediaPrintableArea(
                (pageMiddle - targetMiddle) + 0.25f, 0.17f, 3 - 0.5f,
                5 - (0.17f * 2), INCHES));
        DocAttributeSet docAttributes = new HashDocAttributeSet();
        SimpleDoc doc = new SimpleDoc(createData(), DocFlavor.BYTE_ARRAY.PNG,
                docAttributes);
        job.print(doc, attributes);
    }
    
    private static Object createData() throws Exception
    {
        BufferedImage image = new BufferedImage(300, 500,
                BufferedImage.TYPE_INT_ARGB);
        Graphics2D g = image.createGraphics();
        g.setColor(Color.WHITE);
        g.fillRect(0, 0, 300, 500);
        g.setPaint(new GradientPaint(0, 0, Color.BLACK, 0, 500, Color.WHITE));
        g.fillRect(0, 0, 300, 500);
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        ImageIO.write(image, "PNG", out);
        return out.toByteArray();
    }
}
