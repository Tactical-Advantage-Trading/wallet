package com.sparrowwallet.drongo.protocol;

public abstract class ChildMessage extends Message {

    protected Message parent;

    protected ChildMessage() {
        super();
    }

    protected ChildMessage(byte[] rawtx, int offset) {
        super(rawtx, offset);
    }

    public Message getParent() {
        return parent;
    }

    public final void setParent(Message parent) {
        this.parent = parent;
    }

    protected void adjustLength(int adjustment) {
        adjustLength(0, adjustment);
    }

    protected void adjustLength(int newArraySize, int adjustment) {
        super.adjustLength(newArraySize, adjustment);

        if(parent != null) {
            parent.adjustLength(newArraySize, adjustment);
        }
    }
}
