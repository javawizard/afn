package test;

import javax.print.DocPrintJob;
import javax.print.PrintService;
import javax.print.PrintServiceLookup;
import javax.print.attribute.HashPrintRequestAttributeSet;
import javax.print.attribute.Size2DSyntax;
import javax.print.attribute.standard.MediaPrintableArea;
import javax.print.attribute.standard.MediaSize;

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
        attributes.add(new MediaSize(3, 5, Size2DSyntax.INCH));
        MediaPrintableArea area = (MediaPrintableArea) service
                .getSupportedAttributeValues(MediaPrintableArea.class, null,
                        attributes);
        System.out.println(area.getX(Size2DSyntax.INCH) + " "
                + area.getY(Size2DSyntax.INCH) + " "
                + area.getWidth(Size2DSyntax.INCH) + " "
                + area.getHeight(Size2DSyntax.INCH));
        // job.print(doc, attributes);
    }
}
