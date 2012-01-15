package jw.jzbot.pastebin;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import jw.jzbot.pastebin.PastebinProvider.Feature;

public class PastebinService
{
    private static final ArrayList<PastebinProvider> providers = new ArrayList<PastebinProvider>();
    private static int currentProviderIndex = 0;
    
    public static void installProvider(PastebinProvider provider)
    {
        providers.add(provider);
    }
    
    /**
     * Creates a post. A pastebin provider will be selected from the list of providers.
     * The provider used is guaranteed to have all of the required features.
     * 
     * @param post
     *            The post to create
     * @param requiredFeatures
     *            The features that must be present at the pastebin to be posted to
     * @return The post URL
     */
    public static String createPost(Post post, Feature[] requiredFeatures)
    {
        doFirstTimeInit();
        if (requiredFeatures == null)
            requiredFeatures = new Feature[0];
        if (post == null)
            throw new NullPointerException("Can't create a null post");
        if (post.getData() == null || post.getData().isEmpty())
            throw new PastebinException("Can't create an empty pastebin post");
        List<Feature> requiredFeatureList = Arrays.asList(requiredFeatures);
        for (int i = 0; i < providers.size(); i++)
        {
            currentProviderIndex++;
            currentProviderIndex = currentProviderIndex % providers.size();
            PastebinProvider provider = providers.get(currentProviderIndex);
            if (!Arrays.asList(provider.getSendFeatures()).containsAll(requiredFeatureList))
                /*
                 * This pastebin doesn't contain all of the needed features
                 */
                continue;
            try
            {
                return provider.send(post);
            }
            catch (Exception e)
            {
                System.err.println("Exception for provider " + provider + " (index "
                        + currentProviderIndex);
                e.printStackTrace();
            }
        }
        throw new PastebinException("Tried all pastebin providers (there were "
                + providers.size() + " providers), and all of them threw exceptions or "
                + "didn't have correct features");
    }
    
    private static volatile boolean initialized = false;
    
    private static synchronized void doFirstTimeInit()
    {
        if (initialized)
            return;
        initialized = true;
        currentProviderIndex = (int) (Math.random() * providers.size());
    }
    
    public static Post readPost(String url)
    {
        for (PastebinProvider provider : providers)
        {
            if (provider.understands(url))
                return provider.read(url);
        }
        throw new PastebinException("That pastebin URL (\"" + url
                + "\") is not a valid url.");
    }
    
    public static boolean understands(String url)
    {
        for (PastebinProvider provider : providers)
        {
            if (provider.understands(url))
                return true;
        }
        return false;
    }
    
    public static int getProviderCount()
    {
        return providers.size();
    }
}
