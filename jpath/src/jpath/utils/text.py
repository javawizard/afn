
def trimTo(length, text):
    if len(text) < length:
        return text
    return text[:length] + "..."
