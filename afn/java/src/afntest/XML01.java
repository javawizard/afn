package afntest;

import java.io.StringReader;

import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.xml.sax.InputSource;

public class XML01
{
    public static final String text = "<foo><bar x='baz'/></foo>";
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Throwable
    {
        Document doc =
                DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(
                        new InputSource(new StringReader(text)));
    }
    
}
