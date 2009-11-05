package jw.othello.server;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.IOException;

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
    }
    
    public static Color colorFromHex(String hex)
    {
        if (hex.startsWith("#"))
            hex = hex.substring(1);
        String rs = hex.substring(0, 2);
        String gs = hex.substring(2, 4);
        String bs = hex.substring(4, 6);
        int r = Integer.parseInt(rs, 16);
        int g = Integer.parseInt(gs, 16);
        int b = Integer.parseInt(bs, 16);
        return new Color(r, g, b);
    }
}
