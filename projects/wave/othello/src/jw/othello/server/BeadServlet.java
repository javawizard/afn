package jw.othello.server;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.PrintWriter;

import javax.imageio.ImageIO;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * A servlet that generates bead images based on size, color, and whether or not a board
 * should be included.
 * 
 * @author Alexander Boyd
 * 
 */
public class BeadServlet extends HttpServlet
{
    /**
     * Probably should override doGet and doPost instead (to prevent HEAD, OPTIONS, et
     * cetera, from being sent incorrect responses), but what the heck.
     */
    public void service(HttpServletRequest request, HttpServletResponse response)
            throws IOException
    {
        if (request.getParameter("width") == null)
        {
            response.setHeader("Content-Type", "text/plain");
            PrintWriter out = response.getWriter();
            out.println("You need to specify the following "
                    + "query parameters to the bead servlet:");
            out.println();
            out.println("width: The width of the bead");
            out.println("height: The height of the bead");
            out.println("background: The background color of the image");
            out.println("foreground: The color that the bead should be filled with");
            out.println("outline: The color of the outline of the bead");
            out
                    .println("bordercolor: The color of the border around the edge of the image");
            out.println("borders: A string that specifies which borders to draw. If it "
                    + "contains the letters \"l\", \"t\", \"r\", or \"b\" within it, "
                    + "then the left border, the top border, the right border, and "
                    + "the bottom border will be drawn, respectively. More than one "
                    + "of these letters can appear to draw more than one border.");
            out.println();
            out.println("Colors are specified as a normal HTML hex color string (IE ff0000 " +
            		"is red). The leading # sign is optional. 2 additional characters " +
            		"can be present at the end, which specify the alpha value. 00 is " +
            		"completely transparent, and ff is completely opaque.");
            return;
        }
        int width = Integer.parseInt(request.getParameter("width"));
        int height = Integer.parseInt(request.getParameter("height"));
        Color background = colorFromHex(request.getParameter("background"));
        Color foreground = colorFromHex(request.getParameter("foreground"));
        Color outline = colorFromHex(request.getParameter("outline"));
        Color borderColor = colorFromHex(request.getParameter("bordercolor"));
        String borders = request.getParameter("borders");
        if (borders == null)
            borders = "";
        if (width > 100 || height > 100)
            throw new IOException("Widths and heights in excess of 100 pixels "
                    + "aren't currently allowed.");
        BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
        int ovalX = 3;
        int ovalY = 3;
        int ovalWidth = width - 6;
        int ovalHeight = height - 6;
        Graphics2D g = image.createGraphics();
        g.setColor(background);
        g.fillRect(0, 0, width, height);
        g.setColor(foreground);
        g.fillOval(ovalX, ovalY, ovalWidth, ovalHeight);
        g.setColor(outline);
        g.drawOval(ovalX, ovalY, ovalWidth, ovalHeight);
        g.setColor(borderColor);
        if (borders.contains("t"))
            g.drawLine(0, 0, width, 0);
        if (borders.contains("r"))
            g.drawLine(width-1, 0, width-1, height);
        if (borders.contains("b"))
            g.drawLine(0, height-1, width, height-1);
        if (borders.contains("l"))
            g.drawLine(0, 0, 0, height);
        response.setHeader("Content-Type", "image/png");
        ImageIO.write(image, "PNG", response.getOutputStream());
    }
    
    public static Color colorFromHex(String hex)
    {
        if (hex.startsWith("#"))
            hex = hex.substring(1);
        int a = 255;
        if (hex.length() == 8)
        {
            String as = hex.substring(6, 8);
            a = Integer.parseInt(as, 16);
        }
        String rs = hex.substring(0, 2);
        String gs = hex.substring(2, 4);
        String bs = hex.substring(4, 6);
        int r = Integer.parseInt(rs, 16);
        int g = Integer.parseInt(gs, 16);
        int b = Integer.parseInt(bs, 16);
        return new Color(r, g, b, a);
    }
}
