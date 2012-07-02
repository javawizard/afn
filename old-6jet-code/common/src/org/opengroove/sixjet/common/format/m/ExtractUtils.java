package org.opengroove.sixjet.common.format.m;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.util.Collections;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

public class ExtractUtils
{
    public static void extract(File updatejar, File dest)
    {
        try
        {
            System.out.println("loading jar file");
            ZipFile file = new ZipFile(updatejar);
            System.out.println("about to extract contents");
            byte[] buffer = new byte[4096];
            int amount;
            for (ZipEntry entry : Collections.list(file.entries()))
            {
                System.out.println("extracting entry " + entry.getName());
                File targetFile = new File(dest, entry.getName());
                targetFile.getAbsoluteFile().getParentFile().mkdirs();
                if (!entry.isDirectory())
                {
                    System.out.println("entry is a file.");
                    InputStream stream = file.getInputStream(entry);
                    FileOutputStream fos = new FileOutputStream(targetFile);
                    while ((amount = stream.read(buffer)) != -1)
                    {
                        fos.write(buffer, 0, amount);
                    }
                    fos.flush();
                    fos.close();
                    stream.close();
                }
                System.out.println("extracted entry successfully.");
            }
        }
        catch (Exception e)
        {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
        System.out.println("successfully extracted jar file.");
    }
}
