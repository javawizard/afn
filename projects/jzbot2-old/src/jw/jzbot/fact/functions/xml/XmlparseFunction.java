package jw.jzbot.fact.functions.xml;

import java.io.StringReader;

import jw.jzbot.fact.ArgumentList;
import jw.jzbot.fact.FactContext;
import jw.jzbot.fact.Function;
import jw.jzbot.fact.Sink;
import jw.jzbot.fact.exceptions.FactoidException;

import org.jdom.input.SAXBuilder;

public class XmlparseFunction extends Function
{
    
    @Override
    public void evaluate(Sink sink, ArgumentList arguments, FactContext context)
    {
        try
        {
            context.getXmlDocuments().put(arguments.resolveString(0),
                    new SAXBuilder().build(new StringReader(arguments.resolveString(1))));
        }
        catch (Exception e)
        {
            throw new FactoidException(e);
        }
    }
    
    @Override
    public String getHelp(String topic)
    {
        return "Syntax: {xmlparse|<name>|<text>} -- Parses <text>, which should be "
                + "a valid XML document, and stores it as a document called <name>. "
                + "Within the local scope of this factoid, various {xml<something>} "
                + "functions can be called to get access to stuff within this "
                + "XML document.";
    }
    
}
