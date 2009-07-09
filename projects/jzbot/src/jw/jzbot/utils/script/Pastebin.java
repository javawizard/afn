package jw.jzbot.utils.script;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.CharacterData;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import jw.jzbot.JZBot;

/**
 * Contains methods for creating, reading, and deleting posts from <a
 * href="http://pastebin.com">pastebin.com</a>. JZBot uses this to provide error
 * reports (when an exception occurs, JZBot pastebins the stack trace and then
 * sends a message to the source of the command, containing the error). JZBot
 * also uses this to allow editing of logo scripts, by pastebinning a procedure
 * when "proc edit" is run, and then saving the contents of the pastebin when
 * "proc save" is run.
 * 
 * @author Alexander Boyd
 * 
 */
public class Pastebin
{
    public static enum Duration
    {
        DAY, MONTH, YEAR
    }
    
    /**
     * The maximum size of a pastebin page that will be read before throwing an
     * exception. Currently 150KB.
     */
    private static final int MAX_READ_LENGTH = 1024 * 150;
    
    private static final String START_READ_SEQUENCE = "<textarea id=\"code\" class=\"codeedit\" name=\"code2\" cols=\"80\" rows=\"10\" onkeydown=\"return onTextareaKey(this,event)\">";
    private static final String END_READ_SEQUENCE = "</textarea>";
    
    /**
     * Creates a new pastebin post.
     * 
     * @param poster
     *            The poster name to use
     * @param post
     *            The message of the post. Any lines that start with @@ will
     *            have this removed and will be highlighted in the final paste.
     * @param duration
     *            How long the post should last for
     * @param parent
     *            The id of the post that this one is in reply to, or null or
     *            the empty string if this is a new post, not a reply
     * @return The id of the post. This can be appended to
     *         "http://pastebin.com/" to obtain a url that can be used to view
     *         the post.
     */
    public static String createPost(String poster, String post,
            Duration duration, String parent)
    {
        if (parent == null)
            parent = "";
        try
        {
            URL url = new URL("http://pastebin.com/pastebin.php");
            HttpURLConnection con = (HttpURLConnection) url.openConnection();
            con.setConnectTimeout(JZBot.URL_TIMEOUT);
            con.setReadTimeout(JZBot.URL_TIMEOUT);
            con.setInstanceFollowRedirects(false);
            con.addRequestProperty("Content-type",
                    "application/x-www-form-urlencoded");
            con.setRequestMethod("POST");
            con.setDoOutput(true);
            OutputStream out = con.getOutputStream();
            out
                    .write(("parent_pid=" + URLEncoder.encode(parent)
                            + "&format=text&code2=" + URLEncoder.encode(post)
                            + "&poster=" + URLEncoder.encode(poster)
                            + "&paste=Send&remember=1&expiry="
                            + duration.toString().substring(0, 1).toLowerCase() + "&email=")
                            .getBytes());
            out.flush();
            out.close();
            int responseCode = con.getResponseCode();
            if (responseCode != 302)
                throw new RuntimeException("Received response code "
                        + responseCode + " from pastebin: "
                        + con.getResponseMessage());
            String newUrl = con.getHeaderField("Location");
            if (!newUrl.startsWith("http://pastebin.com/"))
                throw new RuntimeException("Invalid url prefix: " + newUrl);
            return newUrl.substring("http://pastebin.com/".length());
        }
        catch (Exception e)
        {
            throw new RuntimeException(e.getClass().getName() + " "
                    + e.getMessage(), e);
        }
    }
    
    /**
     * Returns the content of the post at the specified url. The post is handed
     * back from pastebin.com with html entities and such to prevent the code
     * from messing up the page; this method properly resolves these back into
     * actual characters so that using
     * {@link #createPost(String, String, Duration, String)} with the exact
     * content returned from this method would result in two posts that are
     * identical (ignoring the sequences of 2 at signs that can be used for
     * highlighting).
     * 
     * @param postUrl
     *            The url of the post
     * @return The text of the specified post
     */
    public static String readPost(String postUrl)
    {
        try
        {
            URL url = new URL(postUrl);
            InputStream stream = url.openStream();
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            byte[] buffer = new byte[512];
            int read = 0;
            while ((read = stream.read(buffer)) != -1)
            {
                if (read > MAX_READ_LENGTH)
                    throw new RuntimeException(
                            "Too many characters read (max is "
                                    + MAX_READ_LENGTH + ")");
                out.write(buffer, 0, read);
            }
            stream.close();
            out.flush();
            out.close();
            String result = new String(out.toByteArray());
            out = null;
            int startSequenceIndex = result.indexOf(START_READ_SEQUENCE);
            if (startSequenceIndex == -1)
                throw new RuntimeException("Start sequence not found in result");
            int endSequenceIndex = result.indexOf(END_READ_SEQUENCE,
                    startSequenceIndex);
            if (endSequenceIndex == -1)
                throw new RuntimeException("End sequence not found in result");
            String resultEncoded = result.substring(startSequenceIndex
                    + START_READ_SEQUENCE.length(), endSequenceIndex);
            /*
             * The document stuff here is to resolve entities, since the
             * pastebin website would have to escape text with entities to
             * prevent the page from interpreting it
             */
            return decodeXml(resultEncoded);
        }
        catch (Exception e)
        {
            throw new RuntimeException(e);
        }
    }
    
    private static String decodeXml(String resultEncoded) throws SAXException,
            IOException, ParserConfigurationException
    {
        Document doc = DocumentBuilderFactory.newInstance()
                .newDocumentBuilder().parse(
                        new InputSource(new StringReader(
                                "<root><textarea id=\"text\">" + resultEncoded
                                        + "</textarea></root>")));
        Element textareaElement = (Element) doc.getDocumentElement()
                .getElementsByTagName("textarea").item(0);
        StringBuffer textResult = new StringBuffer();
        resultEncoded = null;
        NodeList children = textareaElement.getChildNodes();
        for (int i = 0; i < children.getLength(); i++)
        {
            if (children.item(i) instanceof CharacterData)
                textResult.append(((CharacterData) children.item(i)).getData());
        }
        return textResult.toString();
    }
}
