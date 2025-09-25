package com.sparrowwallet.drongo.crypto;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DeterministicHierarchy {
    private final Map<List<ChildNumber>, DeterministicKey> keys = new HashMap<>();
    private final List<ChildNumber> rootPath;
    // Keep track of how many child keys each node has. This is kind of weak.
    private final Map<List<ChildNumber>, ChildNumber> lastChildNumbers = new HashMap<>();

    public DeterministicHierarchy(DeterministicKey rootKey) {
        putKey(rootKey);
        rootPath = rootKey.getPath();
    }

    public final void putKey(DeterministicKey key) {
        List<ChildNumber> path = key.getPath();
        // Update our tracking of what the next child in each branch of the tree should be. Just assume that keys are
        // inserted in order here.
        final DeterministicKey parent = key.getParent();
        if (parent != null)
            lastChildNumbers.put(parent.getPath(), key.getChildNumber());
        keys.put(path, key);
    }

    /**
     * Returns a key for the given path, optionally creating it.
     *
     * @param path the path to the key
     * @return next newly created key using the child derivation function
     * @throws HDDerivationException if create is false and the path was not found.
     */
    public DeterministicKey get(List<ChildNumber> path) throws HDDerivationException {
        if(!keys.containsKey(path)) {
            if(path.size() == 0) {
                throw new IllegalArgumentException("Can't derive the master key: nothing to derive from.");
            }

            DeterministicKey parent = get(path.subList(0, path.size() - 1));
            putKey(HDKeyDerivation.deriveChildKey(parent, path.get(path.size() - 1)));
        }

        return keys.get(path);
    }
}
