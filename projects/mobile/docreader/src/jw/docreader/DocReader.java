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
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.RandomAccessFile;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.Properties;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

import javax.swing.Box;
import javax.swing.BoxLayout;

import jw.docreader.ui.OptionPane;

public class DocReader
{
    public static class OpenDocListener extends MouseAdapter implements ActionListener
    {
        private File docFolder;
        private String docName;
        
        public OpenDocListener(File docFolder, String docName)
        {
            this.docFolder = docFolder;
            this.docName = docName;
        }
        
        @Override
        public void actionPerformed(ActionEvent e)
        {
            loadDocPage(docFolder, 0, docName);
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
    
    private static Font defaultFont = Font.decode(null).deriveFont(10f);
    
    private static int lastVisibleDocPage;
    
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
        showDocListPage(0);
    }
    
    public static void loadDocPage(final File docFolder, final int pageNumber,
            final String docName)
    {
        frame.removeAll();
        try
        {
            Page page = readPage(docFolder, pageNumber);
            if (page == null)
                page = generatePlaceholderEmptyPage();
            boolean backEnabled = pageNumber > 0;
            boolean forwardEnabled = pageNumber < (page.total - 1);
            frame.add(new Label("Page " + (pageNumber + 1) + " of " + page.total + ": "
                    + docName), BorderLayout.NORTH);
            TextArea area = new TextArea(page.text, 0, 0, TextArea.SCROLLBARS_VERTICAL_ONLY);
            area.setEditable(false);
            frame.add(area);
            frame.add(new ButtonPanel(new String[]
            {
                    "back", "forward", "doclist", "find"
            }, new String[]
            {
                    "<", ">", "Library", "Find"
            }, new boolean[]
            {
                    backEnabled, forwardEnabled, true, false
            }, new ButtonListener()
            {
                
                @Override
                public void action(String name)
                {
                    if (name.equals("back"))
                        loadDocPage(docFolder, pageNumber - 1, docName);
                    else if (name.equals("forward"))
                        loadDocPage(docFolder, pageNumber + 1, docName);
                    else if (name.equals("doclist"))
                        showDocListPage(lastVisibleDocPage);
                    else
                        OptionPane.showMessageDialog(frame,
                                "Searching for text within a document is "
                                        + "not currently supported. In the future, "
                                        + "it will be implemented by searching forward "
                                        + "through the document from one character after "
                                        + "the current caret location, going on "
                                        + "to the next "
                                        + "page as necessary, and positioning the caret "
                                        + "at the location where a match was found when "
                                        + "a match is found.");
                }
            }), BorderLayout.SOUTH);
            revalidate(frame);
        }
        catch (Exception e)
        {
            showException(e);
        }
    }
    
    private static Page generatePlaceholderEmptyPage()
    {
        Page page = new Page();
        page.text = "(no pages in this document)";
        page.total = 1;
        return page;
    }
    
    public static Page readPage(File file, int pageNumber) throws ZipException, IOException
    {
        Page page = new Page();
        if (file.isDirectory())
        {
            File pageFile = new File(file, "" + pageNumber);
            if (!pageFile.exists())
                return null;
            page.total = file.listFiles(hiddenFilter).length;
            page.text = readFile(pageFile);
        }
        else
        {
            ZipFile zipFile = new ZipFile(file);
            page.total = zipFile.size();
            ZipEntry entry = zipFile.getEntry("" + pageNumber);
            if (entry == null)
                entry = zipFile.getEntry("/" + pageNumber);
            if (entry == null)
                return null;
            page.text = readStream(zipFile.getInputStream(entry));
        }
        return page;
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
    public static void showDocListPage(int page)
    {
        lastVisibleDocPage = page;
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
            final int pageCount = storage.listFiles(hiddenFilter).length;
            File pageFolder = new File(storage, "" + page);
            boolean allowBack = new File(storage, "" + (page - 1)).exists();
            boolean allowForward = new File(storage, "" + (page + 1)).exists();
            File[] docFolders = pageFolder.listFiles(hiddenFilter);
            Arrays.sort(docFolders);
            frame.add(generateDocListPanel(docFolders));
            frame.add(new Label("Page " + (page + 1) + " of " + pageCount),
                    BorderLayout.NORTH);
            frame.add(new ButtonPanel(new String[]
            {
                    "back", "forward", "page", "search"
            }, new String[]
            {
                    "<", ">", "goto", "search"
            }, new boolean[]
            {
                    allowBack, allowForward, true, true
            }, new ButtonListener()
            {
                
                @Override
                public void action(String name)
                {
                    if (name.equals("back"))
                    {
                        showDocListPage(page - 1);
                    }
                    else if (name.equals("forward"))
                    {
                        showDocListPage(page + 1);
                    }
                    else if (name.equals("page"))
                    {
                        doGotoPlainPage(page, pageCount);
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
    
    protected static void doGotoPlainPage(int page, int pages)
    {
        String newValue = OptionPane.showInputDialog(frame,
                "Enter a page number to go to, from 1 to " + pages + ".", "" + page);
        if (newValue == null)
            return;
        int value;
        try
        {
            value = Integer.parseInt(newValue);
        }
        catch (Exception e)
        {
            OptionPane.showMessageDialog(frame, "That input (\"" + newValue
                    + "\") is not a number.");
            return;
        }
        if (value < 1 || value > pages)
        {
            OptionPane.showMessageDialog(frame,
                    "That number was not within the range 1 to " + pages + ".");
        }
        lastVisibleDocPage = value - 1;
        showDocPlainList(value - 1);
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
        scroll.getVAdjustable().setUnitIncrement(22);
        scroll.getHAdjustable().setUnitIncrement(22);
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
            Panel p2 = new Panel();
            p2.setLayout(new BorderLayout());
            Button button = new Button(name);
            button.setFont(defaultFont.deriveFont(Font.BOLD));
            p2.add(button, BorderLayout.WEST);
            panel.add(p2);
            Label label = new Label(description);
            label.setFont(defaultFont.deriveFont(Font.PLAIN));
            panel.add(label);
            panel.add(verticalSpacer(4));
            button.addActionListener(new OpenDocListener(file, name));
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
    
    public static String readFile(File file)
    {
        try
        {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            FileInputStream fis = new FileInputStream(file);
            copy(fis, baos);
            fis.close();
            baos.flush();
            baos.close();
            return new String(baos.toByteArray(), "UTF-8");
        }
        catch (Exception e)
        {
            throw new RuntimeException(e);
        }
    }
    
    public static String readStream(InputStream stream)
    {
        try
        {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            copy(stream, baos);
            stream.close();
            baos.flush();
            baos.close();
            return new String(baos.toByteArray(), "UTF-8");
        }
        catch (Exception e)
        {
            throw new RuntimeException(e);
        }
    }
    
    public static void writeFile(String string, File file)
    {
        try
        {
            ByteArrayInputStream bais = new ByteArrayInputStream(string.getBytes("UTF-8"));
            FileOutputStream fos = new FileOutputStream(file);
            copy(bais, fos);
            bais.close();
            fos.flush();
            fos.close();
        }
        catch (Exception e)
        {
            throw new RuntimeException(e);
        }
    }
    
    public static void copy(InputStream in, OutputStream out) throws IOException
    {
        byte[] buffer = new byte[8192];
        int amount;
        while ((amount = in.read(buffer)) != -1)
        {
            out.write(buffer, 0, amount);
        }
    }
    
}
