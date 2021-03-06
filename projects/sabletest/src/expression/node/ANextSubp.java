/* This file was generated by SableCC (http://www.sablecc.org/). */

package expression.node;

import expression.analysis.*;

@SuppressWarnings("nls")
public final class ANextSubp extends PSubp
{
    private PMulp _next_;

    public ANextSubp()
    {
        // Constructor
    }

    public ANextSubp(
        @SuppressWarnings("hiding") PMulp _next_)
    {
        // Constructor
        setNext(_next_);

    }

    @Override
    public Object clone()
    {
        return new ANextSubp(
            cloneNode(this._next_));
    }

    public void apply(Switch sw)
    {
        ((Analysis) sw).caseANextSubp(this);
    }

    public PMulp getNext()
    {
        return this._next_;
    }

    public void setNext(PMulp node)
    {
        if(this._next_ != null)
        {
            this._next_.parent(null);
        }

        if(node != null)
        {
            if(node.parent() != null)
            {
                node.parent().removeChild(node);
            }

            node.parent(this);
        }

        this._next_ = node;
    }

    @Override
    public String toString()
    {
        return ""
            + toString(this._next_);
    }

    @Override
    void removeChild(@SuppressWarnings("unused") Node child)
    {
        // Remove child
        if(this._next_ == child)
        {
            this._next_ = null;
            return;
        }

        throw new RuntimeException("Not a child.");
    }

    @Override
    void replaceChild(@SuppressWarnings("unused") Node oldChild, @SuppressWarnings("unused") Node newChild)
    {
        // Replace child
        if(this._next_ == oldChild)
        {
            setNext((PMulp) newChild);
            return;
        }

        throw new RuntimeException("Not a child.");
    }
}
