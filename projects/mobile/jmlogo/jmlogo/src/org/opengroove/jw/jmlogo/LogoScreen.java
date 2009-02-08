package org.opengroove.jw.jmlogo;

/**
 * Represents a logo screen, a device that logo commands can draw onto. Right
 * now, methods are only provided for interacting with one turtle.<br/><br/>
 * 
 * Typically, there is one method for each logo command that interacts with the
 * turtle.
 * 
 * @author Alexander Boyd
 * 
 */
public interface LogoScreen
{
    public Point getPos();
    
    public void hideTurtle();
    
    public void showTurtle();
    
    public void setPos(double x, double y);
    
    public void penDown();
    
    public void penUp();
    
    public void setPenColor(int rgb);
    
    public void setScreenColor(int rgb);
    
    public void forward(double length);
    
    public void right(double degrees);
    
    public void left(double degrees);
    
    public void back(double degrees);
    
    public void home();
    
    public void clean();
    
    public void clearscreen();
    
    public boolean penDownP();
    
    public int getPenColor();
    
    public int getScreenColor();
    
    public double getHeading();
    
    public void setHeading(double heading);
}
