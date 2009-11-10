package jw.docgen;

public class DocModel
{
    private Doc[] docs;
    
    public int getDocCount()
    {
        return docs.length;
    }
    
    public Doc getDoc(int index)
    {
        return docs[index];
    }
    
    public void setDocs(Doc[] docs)
    {
        this.docs = docs;
    }
}
