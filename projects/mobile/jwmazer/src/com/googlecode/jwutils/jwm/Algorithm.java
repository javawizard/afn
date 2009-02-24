package com.googlecode.jwutils.jwm;

import java.util.Random;

public interface Algorithm
{
    /**
     * Draws a newly-generated maze. Random data for the maze should be sourced
     * from the random specified, so that a maze can be re-created by saving the
     * value of the random's seed before the maze is generated.
     * 
     * @param canvas
     *            The canvas to draw on. The maze should be drawn starting in
     *            the lower-left corner.
     * @param width
     *            The width, in cells, of the maze. The maze's width, in pixels,
     *            will be this times <tt>pathsize</tt>.
     * @param height
     *            The height, in cells, of the maze.
     * @param pathsize
     *            The width of the maze's paths. This includes the walls, so a
     *            maze with a width of 10 and a pathsize of 5 would be 50 pixels
     *            wide.
     */
    public void draw(MazerCanvas canvas, int width, int height, int pathsize,
        Random random);
    /**
     * Returns the name of this algorithm. This will be displayed to the user.
     * @return
     */
    public String getName();
}
