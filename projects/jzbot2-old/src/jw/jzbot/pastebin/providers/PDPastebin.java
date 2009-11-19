package jw.jzbot.pastebin.providers;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.net.URLEncoder;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultHttpClient;

import jw.jzbot.pastebin.PastebinProvider;
import jw.jzbot.pastebin.Post;
import jw.jzbot.utils.Pastebin;
import jw.jzbot.utils.Pastebin.Duration;

public class PDPastebin implements PastebinProvider
{
    private String location;
    private boolean subdomains;
    private String downloadFile;
    
    public PDPastebin(String location, boolean subdomains, String downloadFile)
    {
        this.location = location;
        this.subdomains = subdomains;
        this.downloadFile = downloadFile;
    }
    
    @Override
    public Feature[] getReceiveFeatures()
    {
        return new Feature[]
        {};
    }
    
    @Override
    public Feature[] getSendFeatures()
    {
        return new Feature[]
        {
                Feature.author, Feature.forever, Feature.update
        };
    }
    
    @Override
    public Post read(String url)
    {
        try
        {
            url = "http://" + extractSubdomainPrefix(url) + location + "/" + downloadFile + "?dl="
                    + extractId(url);
            HttpClient client = new DefaultHttpClient();
            client.getParams().setParameter("http.socket.timeout", 8000);
            HttpGet request = new HttpGet(url);
            HttpResponse response = client.execute(request);
            InputStream stream = response.getEntity().getContent();
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            byte[] buffer = new byte[512];
            int read = 0;
            while ((read = stream.read(buffer)) != -1)
            {
                out.write(buffer, 0, read);
            }
            stream.close();
            out.flush();
            out.close();
            String result = new String(out.toByteArray(), "US-ASCII");
            return new Post(null, null, null, null, null, result);
        }
        catch (Exception e)
        {
            throw new RuntimeException(e);
        }
    }
    
    private String extractSubdomainPrefix(String url)
    {
        Pattern p = Pattern.compile(getPostPrefixRegex());
        Matcher m = p.matcher(url);
        m.find();
        String value = m.group(1);
        if (value.equals("/") || value.equals(""))
            return "";
        return value;
    }
    
    @Override
    public String send(Post post)
    {
        String parent = post.getParent();
        if (parent == null)
            parent = "";
        String poster = post.getName();
        Duration duration = post.getDuration();
        try
        {
            HttpClient client = new DefaultHttpClient();
            client.getParams().setParameter("http.socket.timeout", 8000);
            HttpPost request = new HttpPost("http://" + location + "/pastebin.php");
            request.addHeader("Content-type", "application/x-www-form-urlencoded");
            request.setEntity(new StringEntity("parent_pid=" + URLEncoder.encode(parent)
                    + "&format=text&code2=" + URLEncoder.encode(post.getData(), "US-ASCII")
                    + "&poster=" + URLEncoder.encode(poster)
                    + "&paste=Send&remember=1&expiry="
                    + duration.toString().substring(0, 1).toLowerCase() + "&email="));
            HttpResponse response = client.execute(request);
            int responseCode = response.getStatusLine().getStatusCode();
            if (responseCode != 302)
                throw new RuntimeException("Received response code " + responseCode
                        + " from pastebin (302 should have been sent instead): "
                        + response.getStatusLine().getReasonPhrase() + " with content "
                        + Pastebin.readContent(response));
            String newUrl = Pastebin.getResponseHeader(response, "Location");
            if (!newUrl.startsWith("http://" + location + "/"))
                throw new RuntimeException("Invalid url prefix: " + newUrl);
            return newUrl;
        }
        catch (Exception e)
        {
            throw new RuntimeException(e.getClass().getName() + " " + e.getMessage(), e);
        }
        
    }
    
    public String extractId(String url)
    {
        Pattern p = Pattern.compile(getPostPrefixRegex());
        Matcher m = p.matcher(url);
        m.find();
        return m.group(2);
    }
    
    @Override
    public boolean understands(String url)
    {
        return url.matches(getPostPrefixRegex());
    }
    
    public String getPostPrefixRegex()
    {
        return "^http\\:/" + (subdomains ? "/([^\\./]*\\.)?" : "(/)") + "\\Q" + location
                + "\\E/([a-zA-Z0-9]*)$";
    }
    
}
