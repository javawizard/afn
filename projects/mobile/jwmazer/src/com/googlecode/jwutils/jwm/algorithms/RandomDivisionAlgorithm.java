package com.googlecode.jwutils.jwm.algorithms;

import java.util.Random;

import javax.microedition.lcdui.Graphics;

import com.googlecode.jwutils.jwm.Algorithm;
import com.googlecode.jwutils.jwm.MazerCanvas;

public class RandomDivisionAlgorithm implements Algorithm
{
    
    public void draw(MazerCanvas canvas, int width, int height, int pathsize,
        Random random)
    {
        Graphics g = canvas.getGraphics();
        g.setColor(0x000000);
        g.drawLine(0, 0, 0, height * pathsize);
        g.drawLine(0, 0, width * pathsize, 0);
        g.drawLine(0, height * pathsize, width * pathsize, height * pathsize);
        g.drawLine(width * pathsize, 0, width * pathsize, height * pathsize);
        canvas.repaint();
        submaze(canvas, 0, 0, width, height, pathsize, random);
        canvas.repaint();
    }
    
    private void submaze(MazerCanvas canvas, int ox, int oy, int w, int h,
        int pathsize, Random random)
    {
        if (w == 1 || h == 1)
            return;
        canvas.repaint();
        Graphics g = canvas.getGraphics();
        g.setColor(0x000000);
        int rwx = randomRange(random, 1, w - 1);
        int rwy = randomRange(random, 1, h - 1);
        int rdx1 = randomRange(random, 1, rwy);
        int rdx2 = randomRange(random, rwy + 1, h);
        int rdy1 = randomRange(random, 1, rwx);
        int rdy2 = randomRange(random, rwx + 1, w);
        int rdomit = randomRange(random, 1, 4);
        int xquadx = ox + (rwx * pathsize);
        int xquady = oy;
        g.drawLine(xquadx, xquady, xquadx, xquady + (h * pathsize));
        if (rdomit != 1)
            etchSegment(g, false, xquadx, xquady, rdx1, pathsize);
        if (rdomit != 2)
            etchSegment(g, false, xquadx, xquady, rdx2, pathsize);
        int yquadx = ox;
        int yquady = oy + (rwy * pathsize);
        g.setColor(0x000000);
        g.drawLine(yquadx, yquady, yquadx + (w * pathsize), yquady);
        if (rdomit != 3)
            etchSegment(g, true, yquadx, yquady, rdy1, pathsize);
        if (rdomit != 4)
            etchSegment(g, true, yquadx, yquady, rdy2, pathsize);
        int xyquadx = xquadx;
        int xyquady = yquady;
        submaze(canvas, ox, oy, rwx, rwy, pathsize, random);
        submaze(canvas, xquadx, xquady, w - rwx, rwy, pathsize, random);
        submaze(canvas, yquadx, yquady, rwx, h - rwy, pathsize, random);
        submaze(canvas, xyquadx, xyquady, w - rwx, h - rwy, pathsize, random);
    }
    
    private void etchSegment(Graphics g, boolean horizontal, int ox, int oy,
        int offset, int pathsize)
    {
        boolean vertical = !horizontal;
        int pixelOffset = (offset - 1) * pathsize;
        g.setColor(0xFFFFFF);
        int x1 = horizontal ? (ox + pixelOffset + 1) : ox;
        int y1 = vertical ? (oy + pixelOffset + 1) : oy;
        int x2 = horizontal ? (ox + pixelOffset + pathsize) - 1 : ox;
        int y2 = vertical ? (oy + pixelOffset + pathsize) - 1 : oy;
        g.drawLine(x1, y1, x2, y2);
        g.setColor(0x000000);
    }
    
    public String getName()
    {
        return "Random Division";
    }
    
    private static int randomRange(Random random, int min, int max)
    {
        int v = random.nextInt(1 + (max - min));
        return v + min;
    }
}
