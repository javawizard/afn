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
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Throwable
    {
        PrintService service = PrintServiceLookup.lookupDefaultPrintService();
        DocPrintJob job = service.createPrintJob();
        HashPrintRequestAttributeSet attributes = new HashPrintRequestAttributeSet();
        attributes.add(MediaSizeName.ISO_B7);
        MediaPrintableArea[] area = (MediaPrintableArea[]) service
                .getSupportedAttributeValues(MediaPrintableArea.class, null,
                        attributes);
        System.out.println(Arrays.asList(area));
        // job.print(doc, attributes);
        System.out.println(MediaSize.findMedia(3, 5, Size2DSyntax.INCH));
    }
}
