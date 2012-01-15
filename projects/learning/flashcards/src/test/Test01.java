package test;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.print.PageFormat;
import java.awt.print.Paper;
import java.awt.print.Printable;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;

public class Test01
{
    public static class TestPrintable implements Printable
    {
        
        @Override
        public int print(Graphics graphics, PageFormat format, int pageIndex)
                throws PrinterException
        {
            if (pageIndex != 0)
                return NO_SUCH_PAGE;
            Graphics2D g = (Graphics2D) graphics;
            g.setFont(Font.decode(null).deriveFont(100f));
            g.setColor(Color.WHITE);
            g.fillRect((int) format.getImageableX(), (int) format
                    .getImageableY(), (int) format.getImageableWidth(),
                    (int) format.getImageableHeight());
            g.setColor(Color.BLACK);
            g.drawString("H", 5, 105);
            return PAGE_EXISTS;
        }
    }
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Throwable
    {
        PrinterJob job = PrinterJob.getPrinterJob();
        PageFormat format = new PageFormat();
        format.setOrientation(format.PORTRAIT);
        Paper paper = new Paper();
        paper.setSize(3 * 72, 5 * 72);
        paper.setImageableArea(0, 0, 1200 * 3, 1200 * 5);
        job.setPrintable(new TestPrintable(), format);
        job.print();
    }
    
}
