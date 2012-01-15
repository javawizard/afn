package jw.docgen;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.HashMap;

public class DocGen
{
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Throwable
    {
        DocModel model = new DocModel();
        Doc doc1 = new Doc();
        doc1.name = "My test doc";
        doc1.description = "This is a test doc just for the heck of it";
        model.setDocs(new Doc[]
        {
            doc1
        });
        generate(model, "/home/amboyd/docs.dfrz");
    }
    
    public static void generate(DocModel model, String filename) throws IOException
    {
        RandomAccessFile file = new RandomAccessFile(filename, "rw");
        file.writeInt(1);
        file.writeInt(model.getDocCount());
        file.seek(8 + (model.getDocCount() * 4));
        HashMap<Integer, Integer> pageMapLocations = new HashMap<Integer, Integer>();
        for (int docNumber = 0; docNumber < model.getDocCount(); docNumber++)
        {
            Doc doc = model.getDoc(docNumber);
            file.writeInt(doc.pages.length);
            pageMapLocations.put(docNumber, (int) file.getFilePointer());
            file.skipBytes(doc.pages.length * 4);
            file.writeUTF(doc.name);
            file.writeUTF(doc.description);
            file.writeUTF(doc.summary);
            file.writeInt(doc.tags.length);
            for (String tag : doc.tags)
            {
                file.writeUTF(tag);
            }
        }
        for (int docNumber = 0; docNumber < model.getDocCount(); docNumber++)
        {
            Doc doc = model.getDoc(docNumber);
            int[] pageIndexes = new int[doc.pages.length];
            int pageMap = pageMapLocations.get(docNumber);
            for (int pageNumber = 0; pageNumber < doc.pages.length; pageNumber++)
            {
                pageIndexes[pageNumber] = (int) file.getFilePointer();
                file.writeUTF(doc.pages[pageNumber]);
            }
            int pos = (int) file.getFilePointer();
            file.seek(pageMap);
            for (int index : pageIndexes)
            {
                file.writeInt(index);
            }
            file.seek(pos);
        }
    }
}
