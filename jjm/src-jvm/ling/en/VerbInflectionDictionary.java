package jjm.ling.en;

import java.io.*;
import java.util.*;

/**
 * Used as a backend for the Inflections class,
 * which provides easy access to verb inflections in Scala code.
 * Borrowed from EasySRL.
 */
public class VerbInflectionDictionary {
    CountDictionary wordDict;
    public ArrayList<String[]> inflections;
    public int[] inflCount;
    public HashMap<String, ArrayList<Integer>> inflMap;

    public VerbInflectionDictionary(CountDictionary wordDict) {
        this.wordDict = wordDict;
        inflections = new ArrayList<>();
        inflMap = new HashMap<>();
    }

    public void loadDictionaryFromFile(String filePath) throws IOException {
        BufferedReader reader;
        reader = new BufferedReader(new InputStreamReader(new FileInputStream(filePath)));
        String line;
        while ((line = reader.readLine()) != null) {
            if (!line.trim().isEmpty()) {
                String[] strs = line.split("\t");
                String[] infl = new String[5];
                boolean inCorpus = false;
                for (int i = 0; i < 5; i++) {
                    infl[i] = strs[i];
                    if (!inCorpus && wordDict.contains(infl[i])) {
                        inCorpus = true;
                    }
                }
                int inflId = inflections.size();
                inflections.add(infl);
                for (int i = 0; i < infl.length; i++) {
                    String v = infl[i];
                    if (v.equals("_") || v.equals("-")) {
                        continue;
                    }
                    if (!inflMap.containsKey(v)) {
                        inflMap.put(v, new ArrayList<>());
                    }
                    ArrayList<Integer> inflIds = inflMap.get(v);
                    if (!inflIds.contains(inflId)) {
                        inflIds.add(inflId);
                    }
                }
            }
        }
        reader.close();
        countInflections();
    }

    public int getBestInflectionId(String verb) {
        ArrayList<Integer> inflIds = inflMap.get(verb);
        if (inflIds == null) {
            return -1;
        }
        int bestId = -1, bestCount = -1;
        for (int i = 0; i < inflIds.size(); i++) {
            int count = inflCount[inflIds.get(i)];
            if (count > bestCount) {
                bestId = inflIds.get(i);
                bestCount = count;
            }
        }
        return bestId;
    }

    public String[] getBestInflections(String verb) {
        String verbPrefix = "";
        if (verb.contains("-")) {
            int idx = verb.indexOf('-');
            verbPrefix = verb.substring(0, idx + 1);
            verb = verb.substring(idx + 1);
        }
        ArrayList<Integer> inflIds = inflMap.get(verb);
        if (inflIds == null) {
            return null;
        }
        int bestId = -1, bestCount = -1;
        for (int i = 0; i < inflIds.size(); i++) {
            int count = inflCount[inflIds.get(i)];
            if (count > bestCount) {
                bestId = inflIds.get(i);
                bestCount = count;
            }
        }
        String[] infl = new String[5];
        for (int i = 0; i < 5; i++) {
            infl[i] = verbPrefix + inflections.get(bestId)[i];
        }
        return infl;
    }

    public String getBestBaseVerb(String verb) {
        int bestId = getBestInflectionId(verb);
        return bestId < 0 ? verb : inflections.get(bestId)[0];
    }

    private void countInflections() {
        inflCount = new int[inflections.size()];
        Arrays.fill(inflCount, 0);
        for (String word : wordDict.getStrings()) {
            int wordCount = wordDict.getCount(word);
            if (inflMap.containsKey(word)) {
                for (int inflId : inflMap.get(word)) {
                    inflCount[inflId] += wordCount;
                }
            }
        }
    }
}
