Let's see... What file format should be used by the keyboard editor stuff...

I'm thinking it'll take the form of a config file. There'll be one section per key, with the section named x-y, where x is the XKB row letter and y is the column starting at the leftmost key on that row that's normally used to type an actual character or symbol (tilde for row 4, A for row 2, for example). This section will have a number of options named a number from 0 to 15; 0-3 will be group 1 level 0-3, 4-7 will be group 2 level 0-3, and so on.

The value of these options will be a base-10 number representing the unicode value of the symbol to be inserted.

I think that's all that's needed for now.

Ok, so the editor itself. It keeps the configuration in-memory. It has some globals specifying the display offset for each row, meaning how many blank keys are inserted at the beginning of the row. Rows 1, 2, and 3 all have this set to 1 at present to offset them to the right since that's how they are on an actual keyboard.

When it loads up, it creates all of the keys. For each key, it creates a text box and ties it to a particular var in the configuration via most likely some sort of function. This function loads the char in the var into the text field and registers a listener on the text field's text state property that saves the var back into the in-memory config when the text field's modified.

There'd then be a save button that saves the config to disk, and a generate function that generates an XKB map from the config (and a button to actually run this function).





