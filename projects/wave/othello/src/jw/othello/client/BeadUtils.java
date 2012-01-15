package jw.othello.client;

public class BeadUtils
{
    // FIXME: change this to be calculated relative to the module base url
    public static String servletUrl = "http://trivergia.com/wavyothello/bead";
    
    /**
     * Generates a URL to the image servlet. Colors must not contain the normal # sign
     * associated with HTML colors, as this method does not currently escape these signs
     * from the resulting url.
     * 
     * @param width
     *            The width of the bead to generate
     * @param height
     *            The height of the bead to generate
     * @param background
     *            The background color
     * @param foreground
     *            The color to fill the bead with
     * @param outline
     *            The color to draw the bead outline with
     * @param bordercolor
     *            The color of the borders around the edge of the image
     * @param borders
     *            A string that specifies which borders to draw. If it contains the
     *            letters "l", "t", "r", or "b" within it, then the left border, the top
     *            border, the right border, and the bottom border will be drawn,
     *            respectively. More than one of these letters can appear to draw more
     *            than one border.
     * 
     * @return A url to the image servlet that will cause it to generate the image
     *         requested by the arguments passed to this method
     */
    public static String generateUrl(int width, int height, String background,
            String foreground, String outline, String bordercolor, String borders)
    {
        return servletUrl + "?width=" + width + "&height=" + height + "&background="
                + background + "&foreground=" + foreground + "&outline=" + outline
                + "&bordercolor=" + bordercolor + "&borders=" + borders;
    }
    
    /**
     * Generates a url to the image servlet that causes it to generate an image of the
     * specified size that is filled entirely with the specified color. In the future,
     * sized divs with a specific background color could probably replace the
     * functionality of this method.
     * 
     * @param width
     *            The width of the image to generate
     * @param height
     *            The height of the image to generate
     * @param color
     *            The color that the image should be
     * @return The url to the image servlet
     */
    public static String generateFilledBoxUrl(int width, int height, String color)
    {
        return generateUrl(width, height, color, color, color, "000000", "");
    }
    
    public static String generateFilledBoxUrl(int width, int height, String color,
            String bordercolor, String borders)
    {
        return generateUrl(width, height, color, color, color, bordercolor, borders);
    }
}
