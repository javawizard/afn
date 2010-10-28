package afn;

import java.math.BigInteger;
import java.security.MessageDigest;

public class Hashutils
{
    /**
     * Computes a number from 0 inclusive to <tt>max</tt> exclusive based on the input
     * string using a hash function, then returns that number.<br/><br/>
     * 
     * The underlying hash function is SHA-1.
     * 
     * @param input
     *            The input to hash
     * @param max
     *            The maximum output value
     * @return The hash
     */
    public static int hashToRange(String input, int max)
    {
        try
        {
            byte[] result = MessageDigest.getInstance("SHA-1").digest(input.getBytes());
            BigInteger number = new BigInteger(1, result);
            number = number.mod(new BigInteger("" + max));
            return number.intValue();
        }
        catch (Exception e)
        {
            throw new RuntimeException(e);
        }
    }
}
