package afn.parcon;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Utils {
    public static <T> List<T> concat(List<T>... lists) {
        ArrayList<T> list = new ArrayList<T>();
        for (List<T> l : lists)
            list.addAll(l);
        return list;
    }
    
    public static <A> List<A> list(A... a) {
        return new ArrayList<A>(Arrays.asList(a));
    }
}
