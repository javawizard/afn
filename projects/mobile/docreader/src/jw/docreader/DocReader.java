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
import java.awt.TextField;
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.Properties;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

import javax.swing.Box;
import javax.swing.BoxLayout;

import jw.docreader.ui.OptionPane;

public class DocReader
{
    public static class DeleteSearchListener implements ActionListener
    {
        private File file;
        
        public DeleteSearchListener(File file)
        {
            this.file = file;
        }
        
        @Override
        public void actionPerformed(ActionEvent e)
        {
            if (OptionPane.showInputDialog(frame,
                    "Are you sure you want to delete the search "
                            + file.getName().substring(0, file.getName().length() - 4)
                            + "?", null) == null)
                return;
            if (!file.delete())
                OptionPane.showMessageDialog(frame, "The file could not be deleted.");
            showSearchChooser();
        }
        
    }
    
    public static class DoSearchActionListener implements ActionListener
    {
        private File file;
        
        public DoSearchActionListener(File file)
        {
            this.file = file;
        }
        
        @Override
        public void actionPerformed(ActionEvent e)
        {
            currentSearchResultFile = file;
            showDocListPage(0);
        }
        
    }
    
    public static class CreateSearchListener implements ActionListener
    {
        private TextField searchField;
        
        public CreateSearchListener(TextField searchField)
        {
            this.searchField = searchField;
        }
        
        @Override
        public void actionPerformed(ActionEvent e)
        {
            if (searchField.getText().equals(""))
            {
                OptionPane.showMessageDialog(frame,
                        "You can't create a builder that doesn't have any text in it.");
                return;
            }
            synchronized (searchBuilderList)
            {
                SearchBuilder builder = new SearchBuilder(searchField.getText());
                searchBuilderList.add(builder);
                builder.start();
            }
            showSearchBuilder();
        }
        
    }
    
    public static class ClearBuilderListener implements ActionListener
    {
        private SearchBuilder builder;
        
        public ClearBuilderListener(SearchBuilder builder)
        {
            this.builder = builder;
        }
        
        @Override
        public void actionPerformed(ActionEvent e)
        {
            synchronized (searchBuilderList)
            {
                searchBuilderList.remove(builder);
            }
            showSearchBuilder();
        }
        
    }
    
    public static class SearchBuilder extends Thread
    {
        public volatile boolean done = false;
        public volatile int currentSearchPage = 0;
        private String searchTerms;
        
        public SearchBuilder(String searchTerms)
        {
            this.searchTerms = searchTerms;
            setPriority(Thread.MIN_PRIORITY);
        }
        
        public void run()
        {
            File searchFile = new File(storageSearch, searchTerms.replace(" ", "-")
                    .replaceAll("[^a-zA-Z0-9\\-]", "")
                    + ".txt");
            String searchTermsLower = searchTerms.toLowerCase();
            try
            {
                FileOutputStream out = new FileOutputStream(searchFile);
                for (int page = 0; page < totalPageCount; page++)
                {
                    currentSearchPage = page;
                    File pageFile = new File(storage, "" + page);
                    ZipFile file = new ZipFile(pageFile);
                    Properties pageProperties = new Properties();
                    pageProperties.load(file.getInputStream(file.getEntry("names.props")));
                    ArrayList<String> docPrefixes = new ArrayList<String>();
                    for (Map.Entry mapEntry : pageProperties.entrySet())
                    {
                        String key = (String) mapEntry.getKey();
                        if (!key.endsWith("n"))
                            continue;
                        // We'll wait 12 milliseconds to give other stuff a chance to work
                        Thread.sleep(12);
                        String currentDocNumber = key.substring(0, key.length() - 1);
                        int docNumber = Integer.parseInt(currentDocNumber);
                        int totalDocPages = Integer.parseInt(pageProperties
                                .getProperty(currentDocNumber + "t"));
                        int matchesInDoc = 0;
                        String docName = pageProperties.getProperty(currentDocNumber + "n");
                        String docDesc = pageProperties.getProperty(currentDocNumber + "d");
                        if (docName != null
                                && docName.toLowerCase().contains(searchTermsLower))
                            matchesInDoc += 1;
                        if (docDesc != null
                                && docDesc.toLowerCase().contains(searchTermsLower))
                            matchesInDoc += 1;
                        for (int docPage = 0; docPage < totalDocPages; docPage++)
                        {
                            Page pageObject = readPage(file, docPage, currentDocNumber
                                    + "/", pageProperties, docNumber);
                            String pageText = pageObject.text.toLowerCase();
                            int previous = 0;
                            boolean hadMatch = true;
                            while (hadMatch)
                            {
                                int matchIndex = pageText.indexOf(searchTermsLower,
                                        previous);
                                if (matchIndex > -1)
                                {
                                    matchesInDoc += 1;
                                    hadMatch = true;
                                    previous = matchIndex + 1;
                                }
                                else
                                {
                                    hadMatch = false;
                                }
                            }
                        }
                        if (matchesInDoc > 0)
                        {
                            out.write(matchesInDoc > 100 ? 100 : matchesInDoc);
                            out.write(page / 100);
                            out.write(page % 100);
                            out.write(docNumber);
                        }
                    }
                }
                out.flush();
                out.close();
            }
            catch (Exception e)
            {
                e.printStackTrace();
                searchFile.delete();
                done = true;
                OptionPane.showMessageDialog(frame,
                        "Error while building search, thrown to parent, see stderr");
            }
            finally
            {
                done = true;
            }
        }
    }
    
    public static class OpenDocListener extends MouseAdapter implements ActionListener
    {
        private ZipFile docFile;
        private String docName;
        private String zipFilePrefix;
        private Properties pageProperties;
        private int docNumber;
        
        public OpenDocListener(ZipFile docFile, String docName, String zipFilePrefix,
                Properties pageProperties, int docNumber)
        {
            this.docFile = docFile;
            this.docName = docName;
            this.zipFilePrefix = zipFilePrefix;
            this.pageProperties = pageProperties;
            this.docNumber = docNumber;
        }
        
        @Override
        public void actionPerformed(ActionEvent e)
        {
            loadDocPage(docFile, 0, docName, zipFilePrefix, pageProperties, docNumber);
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
            String name = pathname.getName();
            if (name.startsWith("."))
                return false;
            if (name.equals("names.props"))
                return false;
            if (name.endsWith("~"))
                return false;
            return true;
        }
    };
    
    public static Frame frame;
    
    public static File storage;
    
    public static int totalDocCount;
    
    public static File currentSearchResultFile;
    
    private static Font defaultFont = Font.decode(null).deriveFont(10f);
    
    private static int lastVisibleDocPage;
    
    public static final Object lock = new Object();
    /**
     * The number of doc names to display per page in the list docs and search docs views.
     */
    private static final int PAGE_LENGTH = 20;
    
    public static Label loadingLabel = new Label("Loading DocReader...");
    
    private static int totalPageCount;
    
    public static ArrayList<SearchBuilder> searchBuilderList = new ArrayList<SearchBuilder>();
    
    private static File storageParent;
    
    private static File storageSearch;
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Throwable
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
        frame.add(loadingLabel);
        frame.show();
        loadingLabel.setText("Searching for docs...");
        revalidate(frame);
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
                    loadingLabel.setText("No doc storage folder. DocReader will exit.");
                    System.out.println("No document storage found at dfrz-docs "
                            + "or ../docs.drfz or ~/docs.dfrz. Exiting.");
                    Thread.sleep(3000);
                    System.exit(0);
                }
            }
        }
        DocReader.storage = f;
        DocReader.storageParent = f.getParentFile();
        DocReader.storageSearch = new File(storageParent, "dfrz-search");
        try
        {
            if (!(DocReader.storageSearch.exists() && DocReader.storageSearch.isDirectory()))
            {
                if (!DocReader.storageSearch.mkdirs())
                {
                    loadingLabel
                            .setText("Searches are disabled due to folder creation failure.");
                    Thread.sleep(4000);
                }
            }
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        loadingLabel.setText("Calculating page count...");
        calculateTotalDocCount();
        loadingLabel.setText("Loading first page...");
        showDocListPage(0);
    }
    
    public static void loadDocPage(final ZipFile docFile, final int pageNumber,
            final String docName, final String zipFilePrefix,
            final Properties pageProperties, final int docNumber)
    {
        // System.out.println("Loading doc page " + pageNumber + " doc " + docName
        // + " prefix " + zipFilePrefix);
        frame.removeAll();
        try
        {
            Page page = readPage(docFile, pageNumber, zipFilePrefix, pageProperties,
                    docNumber);
            if (page == null)
                page = generatePlaceholderEmptyPage();
            boolean backEnabled = pageNumber > 0;
            boolean forwardEnabled = pageNumber < (page.total - 1);
            frame.add(new Label("Page " + (pageNumber + 1) + " of " + page.total + ": "
                    + docName), BorderLayout.NORTH);
            TextArea area = new TextArea(page.text, 0, 0, TextArea.SCROLLBARS_VERTICAL_ONLY);
            area.setEditable(false);
            area.setCaretPosition(0);
            frame.add(area);
            frame.add(new ButtonPanel(new String[]
            {
                    "back", "forward", "doclist", "find"
            }, new String[]
            {
                    "<", ">", "Library", "Find"
            }, new boolean[]
            {
                    backEnabled, forwardEnabled, true, true
            }, new ButtonListener()
            {
                
                @Override
                public void action(String name)
                {
                    if (name.equals("back"))
                        loadDocPage(docFile, pageNumber - 1, docName, zipFilePrefix,
                                pageProperties, docNumber);
                    else if (name.equals("forward"))
                        loadDocPage(docFile, pageNumber + 1, docName, zipFilePrefix,
                                pageProperties, docNumber);
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
            area.setCaretPosition(0);
            // revalidate(frame);
            revalidate(area);
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
    
    public static Page readPage(ZipFile file, int pageNumber, String zipFilePrefix,
            Properties pageProperties, int docNumber) throws ZipException, IOException
    {
        Page page = new Page();
        page.total = Integer.parseInt(pageProperties.getProperty(docNumber + "t"));
        ZipEntry entry = file.getEntry(zipFilePrefix + pageNumber);
        if (entry == null)
            entry = file.getEntry("/" + zipFilePrefix + pageNumber);
        if (entry == null)
            return null;
        page.text = readStream(file.getInputStream(entry));
        return page;
    }
    
    private static void calculateTotalDocCount() throws IOException, InterruptedException
    {
        loadingLabel.setText("Loading page file list...");
        File[] pageFolders = storage.listFiles(hiddenFilter);
        loadingLabel.setText("Running page file loop...");
        int pagesSoFar = 0;
        for (File page : pageFolders)
        {
            // System.gc();
            if ((pagesSoFar++ % 10) == 0)
                loadingLabel.setText("Loading... (p:" + pagesSoFar + " d:" + totalDocCount
                        + ")");
            // ZipFile file = new ZipFile(page);
            // Properties props = new Properties();
            // props.load(file.getInputStream(file.getEntry("names.props")));
            // for (Object key : props.keySet())
            // {
            // if (((String) key).endsWith("n"))
            // totalDocCount += 1;
            // }
            // file.close();
            // add to totalDocCount
        }
        loadingLabel.setText("" + pagesSoFar + " pages");
        totalPageCount = pagesSoFar;
        Thread.sleep(2000);
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
        if (currentSearchResultFile == null)
            showDocPlainList(page);
        else
            showDocSearchList(page);
    }
    
    public static void showDocPlainList(final int page)
    {
        frame.removeAll();
        Label statusLabel = new Label();
        frame.add(statusLabel);
        statusLabel.setText("Garbage collecting...");
        revalidate(frame);
        System.gc();
        try
        {
            File pageFile = new File(storage, "" + page);
            statusLabel.setText("Opening zip file...");
            ZipFile file = new ZipFile(pageFile);
            boolean allowBack = new File(storage, "" + (page - 1)).exists();
            boolean allowForward = new File(storage, "" + (page + 1)).exists();
            Properties pageProperties = new Properties();
            statusLabel.setText("Loading page properties...");
            pageProperties.load(file.getInputStream(file.getEntry("names.props")));
            statusLabel.setText("Iterating over page properties...");
            ArrayList<String> docPrefixes = new ArrayList<String>();
            for (Map.Entry mapEntry : pageProperties.entrySet())
            {
                // list page properties, add to doc prefixes, sort, then add trailing
                // slash
                String key = (String) mapEntry.getKey();
                if (!key.endsWith("n"))
                    continue;
                docPrefixes.add(key.substring(0, key.length() - 1));
            }
            statusLabel.setText("Sorting doc prefixes...");
            Collections.sort(docPrefixes);
            statusLabel.setText("Creating doc prefix array...");
            String[] prefixes = docPrefixes.toArray(new String[0]);
            for (int i = 0; i < prefixes.length; i++)
            {
                prefixes[i] = prefixes[i] + "/";
            }
            statusLabel.setText("Loading doc list component...");
            Component docListComponent = generateDocListPanel(Collections.nCopies(
                    prefixes.length, file).toArray(new ZipFile[0]), prefixes, statusLabel,
                    null);
            frame.removeAll();
            frame.add(docListComponent);
            frame.add(new Label("Page " + (page + 1) + " of " + totalPageCount),
                    BorderLayout.NORTH);
            frame.add(new ButtonPanel(new String[]
            {
                    "back", "forward", "page", "build", "search", "random", "help"
            }, new String[]
            {
                    "<", ">", "G", "B", "S", "R", "?"
            }, new boolean[]
            {
                    allowBack, allowForward, true, true, true, true, true
            }, new ButtonListener()
            {
                
                @Override
                public void action(String name)
                {
                    if (name.equals("help"))
                    {
                        OptionPane
                                .showMessageDialog(
                                        frame,
                                        "Buttons:\n\n"
                                                + "<  --  Go back one page.\n"
                                                + ">  --  Go forward one page.\n"
                                                + "G  --  Enter a page number to go to.\n"
                                                + "B  --  Specify a search query, and build a search from it. This also lets you "
                                                + "view searches currently being built.\n"
                                                + "S  --  View a search that you have built.\n"
                                                + "R  --  Choose a page at random and go to it.\n"
                                                + "?  --  View this help page.\n\n"
                                                + "On other pages, you may see these:\n\n"
                                                + "L  --  Return to the library (the list of pages you were just at before you hit the ?)\n"
                                                + "F  --  Refresh the current page and any information shown on it.\n\n"
                                                + "On the doc list page, the number in parentheses before each doc title is the number "
                                                + "of pages in the doc. On the search page, the number in square brackets is the number "
                                                + "of times the search query was found within the document.");
                    }
                    else if (name.equals("back"))
                    {
                        showDocListPage(page - 1);
                    }
                    else if (name.equals("forward"))
                    {
                        showDocListPage(page + 1);
                    }
                    else if (name.equals("page"))
                    {
                        doGotoPlainPage(page, totalPageCount);
                    }
                    else if (name.equals("random"))
                    {
                        showDocPlainList((int) (Math.random() * totalPageCount));
                    }
                    else if (name.equals("build"))
                    {
                        showSearchBuilder();
                    }
                    else if (name.equals("search"))
                    {
                        showSearchChooser();
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
    
    protected static void showSearchChooser()
    {
        frame.removeAll();
        Panel panel = new Panel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        ScrollPane scroll = new ScrollPane();
        scroll.add(panel);
        frame.add(scroll);
        File[] files = storageSearch.listFiles(hiddenFilter);
        for (File file : files)
        {
            String fileName = file.getName();
            if (!fileName.endsWith(".txt"))
                continue;
            fileName = fileName.substring(0, fileName.length() - 4);
            Button searchButton = new Button(fileName);
            searchButton.addActionListener(new DoSearchActionListener(file));
            Button deleteButton = new Button("Delete");
            deleteButton.addActionListener(new DeleteSearchListener(file));
            panel.add(searchButton);
            panel.add(deleteButton);
            panel.add(verticalSpacer(8));
            
        }
        Button libraryButton = new Button("Back to the library");
        libraryButton.addActionListener(new ActionListener()
        {
            
            @Override
            public void actionPerformed(ActionEvent e)
            {
                showDocListPage(lastVisibleDocPage);
            }
        });
        panel.add(libraryButton);
        revalidate(frame);
    }
    
    protected static void showSearchBuilder()
    {
        frame.removeAll();
        Panel panel = new Panel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        panel.add(new Label("Type some search terms:"));
        TextField searchField = new TextField(20);
        panel.add(searchField);
        Button createSearchButton = new Button("Build a search from these terms");
        panel.add(createSearchButton);
        panel.add(new Label("Current search builders:"));
        panel.add(new Label(" "));
        synchronized (searchBuilderList)
        {
            for (SearchBuilder builder : searchBuilderList)
            {
                panel.add(new Label(builder.searchTerms));
                if (!builder.done)
                {
                    panel.add(new Label("--> On page " + builder.currentSearchPage));
                }
                else
                // if(builder.done)
                {
                    panel.add(new Label("--> Done."));
                    Button clearBuilderButton = new Button("Remove from this list");
                    panel.add(clearBuilderButton);
                    clearBuilderButton.addActionListener(new ClearBuilderListener(builder));
                }
            }
        }
        createSearchButton.addActionListener(new CreateSearchListener(searchField));
        Button libraryButton = new Button("Back to the library");
        panel.add(libraryButton);
        libraryButton.addActionListener(new ActionListener()
        {
            
            @Override
            public void actionPerformed(ActionEvent e)
            {
                showDocListPage(lastVisibleDocPage);
            }
        });
        ScrollPane scroll = new ScrollPane();
        scroll.add(panel);
        frame.add(scroll);
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
    
    private static void revalidate(Component container)
    {
        container.invalidate();
        container.validate();
        container.repaint();
    }
    
    public static final int SEARCH_PAGE_SIZE = 10;
    
    public static void showDocSearchList(final int page)
    {
        frame.removeAll();
        revalidate(frame);
        try
        {
            int resultFileLength = (int) currentSearchResultFile.length();
            int numberOfResults = resultFileLength / 4;
            FileInputStream in = new FileInputStream(currentSearchResultFile);
            int offsetItem = page * SEARCH_PAGE_SIZE;
            int itemsOnThisPage = numberOfResults - offsetItem;
            if (itemsOnThisPage > SEARCH_PAGE_SIZE)
                itemsOnThisPage = SEARCH_PAGE_SIZE;
            int offsetInBytes = offsetItem * 4;
            skip(in, offsetInBytes);
            ArrayList<ZipFile> zipFiles = new ArrayList<ZipFile>();
            ArrayList<String> prefixList = new ArrayList<String>();
            ArrayList<String> beforeTitleList = new ArrayList<String>();
            int lastPageNumber = -1;
            ZipFile lastZipFile = null;
            for (int itemNumber = 0; itemNumber < itemsOnThisPage; itemNumber++)
            {
                int matchCount = in.read();
                int pageNumber = in.read() * 100;
                pageNumber += in.read();
                int docNumber = in.read();
                beforeTitleList.add("[" + matchCount + "] ");
                prefixList.add(docNumber + "/");
                if (pageNumber != lastPageNumber)
                {
                    lastPageNumber = pageNumber;
                    lastZipFile = new ZipFile(new File(storage, "" + pageNumber));
                }
                zipFiles.add(lastZipFile);
            }
            frame.add(generateDocListPanel(zipFiles.toArray(new ZipFile[0]), prefixList
                    .toArray(new String[0]), null, beforeTitleList.toArray(new String[0])));
            String searchName = currentSearchResultFile.getName();
            searchName = searchName.substring(0, searchName.length() - 4);
            final int totalPages;
            if (numberOfResults == 0)
                totalPages = 0;
            else
                totalPages = ((numberOfResults - 1) / SEARCH_PAGE_SIZE) + 1;
            frame.add(new Label("" + (page + 1) + " of " + totalPages + ": " + searchName),
                    BorderLayout.NORTH);
            frame.add(new ButtonPanel(new String[]
            {
                    "back", "forward", "go", "library"
            }, new String[]
            {
                    "<", ">", "G", "L"
            }, new boolean[]
            {
                    page > 0, page < (totalPages - 1), true, true
            }, new ButtonListener()
            {
                
                @Override
                public void action(String name)
                {
                    if (name.equals("back"))
                        showDocListPage(page - 1);
                    else if (name.equals("forward"))
                        showDocListPage(page + 1);
                    else if (name.equals("library"))
                    {
                        currentSearchResultFile = null;
                        showDocListPage(0);
                    }
                    else if (name.equals("go"))
                    {
                        String newValue = OptionPane.showInputDialog(frame,
                                "Enter a page number to go to, from 1 to " + totalPages
                                        + ".", "" + page);
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
                        if (value < 1 || value > totalPages)
                        {
                            OptionPane.showMessageDialog(frame,
                                    "That number was not within the range 1 to "
                                            + totalPages + ".");
                        }
                        showDocListPage(value - 1);
                    }
                }
            }), BorderLayout.SOUTH);
        }
        catch (Exception e)
        {
            showException(e);
            currentSearchResultFile = null;
            showDocListPage(0);
        }
        revalidate(frame);
    }
    
    /**
     * Reads <tt>count</tt> bytes from <tt>in</tt> and discards them. This method silently
     * returns if the end of the stream is encountered before this many bytes have been
     * skipped over.
     * 
     * @param in
     *            The input stream to read from
     * @param count
     *            The number of bytes to skip
     */
    public static void skip(InputStream in, int count) throws IOException
    {
        if (count == 0)
            return;
        byte[] buffer = new byte[512];
        int amount = 0;
        while (count > 0)
        {
            amount = in.read(buffer, 0, (count > buffer.length ? buffer.length : count));
            if (amount < 0)// EOF
                return;
            if (amount == 0)
                throw new RuntimeException(
                        "Invalid read data; 0 bytes are reported to have been read. "
                                + "This would cause an infinite loop.");
            count -= amount;
        }
    }
    
    /**
     * Generates a panel that shows the document list for the specified documents, and
     * returns the list.
     * 
     * @param docFolders
     * @throws IOException
     */
    public static Component generateDocListPanel(ZipFile[] files, String[] prefixes,
            Label statusLabel, String[] beforeTitles) throws IOException
    {
        ScrollPane scroll = new ScrollPane();
        scroll.getVAdjustable().setUnitIncrement(22);
        scroll.getHAdjustable().setUnitIncrement(22);
        Panel panel = new Panel();
        scroll.add(panel);
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        String lastPageName = null;
        Properties lastPageProperties = new Properties();
        for (int i = 0; i < files.length; i++)
        {
            ZipFile file = files[i];
            String filePrefix = prefixes[i];
            String pageName = lastComponent(file.getName());
            String fileNumber = filePrefix.substring(0, filePrefix.length() - 1);
            if (!pageName.equals(lastPageName))
            {
                /*
                 * This must be set to a new properties object, not cleared, since a
                 * reference to this is passed to the instance of OpenDocActionListener,
                 * which needs the properties when the button is clicked.
                 */
                // System.out.println("Loading page props");
                lastPageProperties = new Properties();
                lastPageProperties.load(file.getInputStream(file.getEntry("names.props")));
                lastPageName = pageName;
            }
            String name = lastPageProperties.getProperty(fileNumber + "n");
            String description = lastPageProperties.getProperty(fileNumber + "d");
            if (name == null)
                name = "(no name)";
            if (description == null)
                description = "(no description)";
            if (beforeTitles != null)
                name = beforeTitles[i] + name;
            Panel p2 = new Panel();
            p2.setLayout(new BorderLayout());
            Button button = new Button("("
                    + lastPageProperties.getProperty(fileNumber + "t") + ") " + name);
            button.setFont(defaultFont.deriveFont(Font.BOLD));
            p2.add(button, BorderLayout.WEST);
            panel.add(p2);
            Label label = new Label(description);
            label.setFont(defaultFont.deriveFont(Font.PLAIN));
            panel.add(label);
            panel.add(verticalSpacer(4));
            button.addActionListener(new OpenDocListener(file, name, filePrefix,
                    lastPageProperties, Integer.parseInt(fileNumber)));
        }
        return scroll;
    }
    
    private static String lastComponent(String name)
    {
        return new File(name).getName();
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
