package org.opengroove.jw.jmlogo;

import henson.midp.Float11;

import javax.microedition.lcdui.Canvas;
import javax.microedition.lcdui.Graphics;
import javax.microedition.lcdui.Image;

public class LogoCanvas extends Canvas implements LogoScreen
{
    private double turtleX = 0;
    private double turtleY = 0;
    private double turtleHeading = 0;
    
    private static double turtleWidth = 12;
    private static double turtleHalfWidth = turtleWidth / 2.0;
    
    private int turtleColor = 0x000000;
    
    private int screenColor = 0xFFFFFF;
    
    private double xTrans;
    private double yTrans;
    
    private boolean isPenDown = true;
    private boolean isTurtleShowing = true;
    
    private Image buffer;
    private Graphics graphics;
    
    private int bufferWidth;
    private int bufferHeight;
    
    private final Point futurePoint = new Point();
    
    /**
     * This should be called once the logo canvas has been displayed for the
     * first time. It creates the offscreen image used to render to this canvas.
     * It creates an offscreen image that is the size of this component, so it
     * should be displayed and laid out in it's final size before this method is
     * called.
     */
    public void init()
    {
        buffer = Image.createImage(getWidth(), getHeight());
        bufferWidth = getWidth();
        bufferHeight = getHeight();
        graphics = buffer.getGraphics();
        xTrans = getWidth() / 2;
        yTrans = getWidth() / 2;
    }
    
    public void paint(Graphics cg)
    {
        int width = getWidth();
        int height = getHeight();
        /*
         * Draw the buffer onto the screen
         */
        cg.drawImage(buffer, 0, 0, cg.TOP | cg.LEFT);
        /*
         * Draw the turtle onto the screen
         */
        if (isTurtleShowing)
            drawTurtle(cg);
        /*
         * Draw the border
         */
        cg.setColor(0x00FFFFFF);
        cg.drawRect(0, 0, width - 1, height - 1);
        cg.setColor(0x00000000);
        cg.drawRect(1, 1, width - 3, height - 3);
    }
    
    private void drawTurtle(Graphics cg)
    {
        Point turtleResult = new Point();
        anglePoint(turtleX, turtleY, turtleHeading + 90, turtleHalfWidth, turtleResult);
        double turtleRightX = gx(turtleResult.x);
        double turtleRightY = gy(turtleResult.y);
        anglePoint(turtleX, turtleY, turtleHeading, turtleHalfWidth, turtleResult);
        double turtleFrontX = gx(turtleResult.x);
        double turtleFrontY = gy(turtleResult.y);
        anglePoint(turtleX, turtleY, turtleHeading - 90, turtleHalfWidth, turtleResult);
        double turtleLeftX = gx(turtleResult.x);
        double turtleLeftY = gy(turtleResult.y);
        cg.setColor(turtleColor);
        cg.drawLine((int) turtleLeftX, (int) turtleLeftY, (int) turtleRightX,
            (int) turtleRightY);
        cg.drawLine((int) turtleLeftX, (int) turtleLeftY, (int) turtleFrontX,
            (int) turtleFrontY);
        cg.drawLine((int) turtleRightX, (int) turtleRightY, (int) turtleFrontX,
            (int) turtleFrontY);
    }
    
    /**
     * Calculates the point that an entity would arrive at were that entity to
     * start at the x and y position named, face towards the angle (in degrees)
     * specified, and travel the distance specified. Positive Y is up, positive
     * X is right (following the standard logo coordinate space). 0 degrees is
     * facing up (along +Y), 90 degrees is facing right (along +X).
     * 
     * @param x
     *            The x coordinate to start at
     * @param y
     *            The y coordinate to start at
     * @param angle
     * @param distance
     * @param result
     *            The point in which to store the result
     */
    private void anglePoint(double x, double y, double angle, double distance,
        Point result)
    {
        double theta = Math.toRadians(angle);
        double radius = distance;
        double ny = radius * Math.cos(theta);
        double nx = radius * Math.sin(theta);
        result.x = x + nx;
        result.y = y + ny;
    }
    
    public void back(double length)
    {
        forward(-length);
    }
    
    public void forward(double length)
    {
        System.out.println("moving forward, cx:" + turtleX + ",cy:" + turtleY + ",l:"
            + length);
        turtleAnglePoint(length, futurePoint);
        double futureX = futurePoint.x;
        double futureY = futurePoint.y;
        System.out.println("fx:" + futureX + ",fy:" + futureY);
        if (isPenDown)
        {
            graphics.drawLine((int) gx(turtleX), (int) gy(turtleY), (int) gx(futureX),
                (int) gy(futureY));
        }
        turtleX = futureX;
        turtleY = futureY;
        repaint();
    }
    
    /**
     * Same as {@link #anglePoint(double, double, double, double, Point)}, but
     * uses the turtle's current position as the first two arguments.
     * 
     * @param angle
     * @param distance
     * @param result
     */
    public void turtleAnglePoint(double angle, double distance, Point result)
    {
        anglePoint(turtleX, turtleY, angle, distance, result);
    }
    
    /**
     * Same as {@link #turtleAnglePoint(double, double, Point)}, but uses the
     * turtle's current heading as the first argument.
     * 
     * @param distance
     * @param result
     */
    public void turtleAnglePoint(double distance, Point result)
    {
        turtleAnglePoint(turtleHeading, distance, result);
    }
    
    public Point getPos()
    {
        Point p = new Point();
        p.x = turtleX;
        p.y = turtleY;
        return p;
    }
    
    public void hideTurtle()
    {
        isTurtleShowing = false;
        repaint();
    }
    
    public void left(double degrees)
    {
        turtleHeading -= degrees;
        roundTurtleHeading();
        repaint();
    }
    
    public void penDown()
    {
        isPenDown = true;
    }
    
    public void penUp()
    {
        isPenDown = false;
    }
    
    public void right(double degrees)
    {
        turtleHeading += degrees;
        roundTurtleHeading();
        repaint();
    }
    
    private void roundTurtleHeading()
    {
        if (turtleHeading < 0.0 && turtleHeading >= 360.0)
            turtleHeading = turtleHeading % 360.0;
    }
    
    public void setPos(double x, double y)
    {
        turtleX = x;
        turtleY = y;
        repaint();
    }
    
    public void showTurtle()
    {
        isTurtleShowing = true;
        repaint();
    }
    
    /**
     * Converts from logo coordinates to screen coordinates.
     * 
     * @param x
     * @return
     */
    private double gx(double x)
    {
        x += xTrans;
        return x;
    }
    
    /**
     * Converts from logo coordinates to screen coordinates.
     * 
     * @param y
     * @return
     */
    private double gy(double y)
    {
        y *= -1;
        y += yTrans;
        return y;
    }
    
    public void clean()
    {
        int color = graphics.getColor();
        graphics.setColor(screenColor);
        graphics.fillRect(0, 0, bufferWidth, bufferHeight);
        graphics.setColor(color);
        repaint();
    }
    
    public void clearscreen()
    {
        home();
        clean();
    }
    
    public int getPenColor()
    {
        return graphics.getColor();
    }
    
    public int getScreenColor()
    {
        return screenColor;
    }
    
    public void home()
    {
        if (isPenDown)
        {
            graphics.drawLine((int) gx(0), (int) gy(0), (int) gx(turtleX),
                (int) gy(turtleY));
        }
        turtleX = 0;
        turtleY = 0;
        turtleHeading = 0;
        repaint();
    }
    
    public boolean penDownP()
    {
        return isPenDown;
    }
    
    public void setPenColor(int rgb)
    {
        graphics.setColor(rgb);
    }
    
    public void setScreenColor(int rgb)
    {
        System.out.println("setting screen color to " + Integer.toHexString(rgb));
        screenColor = rgb;
        clean();
    }
    
    public double getHeading()
    {
        return turtleHeading;
    }
    
    public void setHeading(double heading)
    {
        turtleHeading = heading;
        roundTurtleHeading();
        repaint();
    }
    
    public double towards(double x, double y)
    {
        /*
         * X and Y are intentionally reversed as arguments to Float11.atan2,
         * since they are exactly swapped in logo coordinate space from
         * mathematical coordinate space.
         */
        return Float11.atan2(x, y);
    }
    
}
