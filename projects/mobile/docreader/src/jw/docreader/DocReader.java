package jw.docreader;

import java.awt.BorderLayout;
import java.awt.Button;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Label;
import java.awt.Panel;
import java.awt.ScrollPane;
import java.awt.TextArea;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.RandomAccessFile;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.Properties;

import javax.swing.Box;
import javax.swing.BoxLayout;

import jw.docreader.ui.OptionPane;

public class DocReader
{
    public static class OpenDocListener extends MouseAdapter implements ActionListener
    {
        public OpenDocListener(File docFolder)
        {
        }
        
        @Override
        public void actionPerformed(ActionEvent e)
        {
        }
        
        @Override
        public void mouseClicked(MouseEvent e)
        {
            actionPerformed(null);
        }
        
    }
    
    /**
     * Filters out all hidden files, files that start with ".", files that end with "~",
     * and files named "names.props".
     */
    public static FileFilter hiddenFilter = new FileFilter()
    {
        
        @Override
        public boolean accept(File pathname)
        {
            if (pathname.isHidden())
                return false;
            if (pathname.getName().startsWith("."))
                return false;
            if (pathname.getName().equals("names.props"))
                return false;
            if (pathname.getName().endsWith("~"))
                return false;
            return true;
        }
    };
    
    public static Frame frame;
    
    public static File storage;
    
    public static int totalDocCount;
    /**
     * A string of file paths to the folders representing each document that matched the
     * last search query
     */
    public static String[] currentSearchResults;
    
    private static Font defaultFont = Font.decode(null);
    
    public static final Object lock = new Object();
    /**
     * The number of doc names to display per page in the list docs and search docs views.
     */
    private static final int PAGE_LENGTH = 20;
    
    /**
     * @param args
     */
    public static void main(String[] args) throws IOException
    {
        frame = new Frame("DocReader");
        frame.setSize(240, 320);
        frame.addWindowListener(new WindowAdapter()
        {
            
            @Override
            public void windowClosing(WindowEvent e)
            {
                frame.dispose();
            }
        });
        frame.add(new Label("Loading DocReader..."));
        frame.show();
        File f = new File("drfz-docs");
        System.out.println("Checking for " + f.getCanonicalPath());
        if (!f.exists())
        {
            f = new File((new File(System.getProperty("user.dir")).getParentFile()),
                    "dfrz-docs");
            System.out.println("Checking for " + f.getCanonicalPath());
            if (!f.exists())
            {
                f = new File(System.getProperty("user.home"), "dfrz-docs");
                System.out.println("Checking for " + f.getCanonicalPath());
                if (!f.exists())
                {
                    System.out.println("No document storage found at dfrz-docs "
                            + "or ../docs.drfz or ~/docs.dfrz. Exiting.");
                    System.exit(0);
                }
            }
        }
        DocReader.storage = f;
        calculateTotalDocCount();
        showDocPlainList(0);
    }
    
    private static void calculateTotalDocCount()
    {
        File[] pageFolders = storage.listFiles(hiddenFilter);
        for (File page : pageFolders)
        {
            totalDocCount += page.listFiles(hiddenFilter).length;
        }
    }
    
    /**
     * Shows the doc list page. This delegates to showDocPlainList or showDocSearchList
     * dependong on whether currentSearchResults is null or not.
     * 
     * @param page
     *            The page number to show. 0 is the first page.
     * @throws IOException
     */
    public static void showDocListPage(int page) throws IOException
    {
        if (currentSearchResults == null)
            showDocPlainList(page);
        else
            showDocSearchList(page);
    }
    
    public static void showDocPlainList(final int page)
    {
        frame.removeAll();
        try
        {
            File pageFolder = new File(storage, "" + page);
            boolean allowBack = new File(storage, "" + (page - 1)).exists();
            boolean allowForward = new File(storage, "" + (page + 1)).exists();
            File[] docFolders = pageFolder.listFiles(hiddenFilter);
            Arrays.sort(docFolders);
            frame.add(generateDocListPanel(docFolders));
            frame.add(new Label("Page " + (page + 1) + " of "
                    + storage.listFiles(hiddenFilter).length), BorderLayout.NORTH);
            frame.add(new ButtonPanel(new String[]
            {
                    "back", "forward", "search"
            }, new String[]
            {
                    "<<", ">>", "search"
            }, new boolean[]
            {
                    allowBack, allowForward, true
            }, new ButtonListener()
            {
                
                @Override
                public void action(String name)
                {
                    if (name.equals("back"))
                    {
                        showDocPlainList(page - 1);
                    }
                    else if (name.equals("forward"))
                    {
                        showDocPlainList(page + 1);
                    }
                    else
                    {
                        OptionPane.showMessageDialog(frame,
                                "Searching isn't yet supported.");
                    }
                }
            }), BorderLayout.SOUTH);
        }
        catch (Exception e)
        {
            showException(e);
        }
        revalidate(frame);
    }
    
    private static void showException(Exception e)
    {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        e.printStackTrace(pw);
        OptionPane.showMessageDialog(frame, "An internal error occured:\n\n"
                + sw.toString());
    }
    
    private static void revalidate(Container container)
    {
        container.invalidate();
        container.validate();
        container.repaint();
    }
    
    public static void showDocSearchList(int page)
    {
        
    }
    
    /**
     * Generates a panel that shows the document list for the specified documents, and
     * returns the list.
     * 
     * @param docFolders
     * @throws IOException
     */
    public static Component generateDocListPanel(File[] docFolders) throws IOException
    {
        ScrollPane scroll = new ScrollPane();
        Panel panel = new Panel();
        scroll.add(panel);
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        String lastPageName = null;
        Properties lastPageProperties = new Properties();
        for (File file : docFolders)
        {
            String pageName = file.getParentFile().getName();
            if (!pageName.equals(lastPageName))
            {
                lastPageProperties.clear();
                lastPageProperties.load(new FileInputStream(new File(file.getParentFile(),
                        "names.props")));
            }
            String name = lastPageProperties.getProperty(file.getName() + "n");
            String description = lastPageProperties.getProperty(file.getName() + "d");
            if (name == null)
                name = "(no name)";
            if (description == null)
                description = "(no description)";
            Button button = new Button(name);
            button.setFont(defaultFont.deriveFont(Font.BOLD));
            panel.add(button);
            Label label = new Label(description);
            label.setFont(defaultFont.deriveFont(Font.PLAIN));
            panel.add(label);
            panel.add(verticalSpacer(4));
            button.addActionListener(new OpenDocListener(file));
        }
        return scroll;
    }
    
    private static Component verticalSpacer(final int height)
    {
        return new Label("")
        {
            
            @Override
            public Dimension getMaximumSize()
            {
                return new Dimension(10000, height);
            }
            
            @Override
            public Dimension getMinimumSize()
            {
                return new Dimension(0, height);
            }
            
            @Override
            public Dimension getPreferredSize()
            {
                return new Dimension(2, height);
            }
        };
    }
}
