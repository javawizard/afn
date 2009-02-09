package org.opengroove.jw.jmlogo;

import javax.microedition.rms.RecordFilter;

public class ProcedureRecordFilter implements RecordFilter
{
    public boolean matches(byte[] candidate)
    {
        if (candidate == null)
            return false;
        return candidate.length > 0 && candidate[0] == 'p';
    }
}
