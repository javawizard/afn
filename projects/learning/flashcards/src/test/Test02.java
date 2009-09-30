package test;

import java.util.Arrays;

import javax.print.DocPrintJob;
import javax.print.PrintService;
import javax.print.PrintServiceLookup;
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
        System.out.println(pageWidth);
    }
}
