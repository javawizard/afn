package afn.libautobus.proxies;

import java.util.Map;

public interface TimerInterface
{
    public int create();
    
    public void set_time(int timer_number, int new_absolute_time);
    
    public void update_time(int timer_number, int amount_to_add);
    
    public void set(int timer_number, Map<String, ?> attributes);
    
    public void set_attribute(int timer_number, String name, Object value);
    
    public void delete(int timer_number);
    
    public void announce(int number);
}
