for file in $(cat data/curr_texts.txt); do
    echo "data/02_rezrDF/$file.Rdata" | sed 's/\r//g';
done | tr '\n' ' ' > data/curr_final_anno_docs.txt