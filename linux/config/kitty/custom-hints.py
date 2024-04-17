import re
import os

def mark(text, args, Mark, extra_cli_args, *a):
    # This function is responsible for finding all
    # matching text. extra_cli_args are any extra arguments
    # passed on the command line when invoking the kitten.
    # We mark all individual word for potential selection
    pattern = r"[/\.a-zA-Z0-9][/\-\_\.A-Za-z0-9]{3,}(:\d+)?"

    for idx, m in enumerate(re.finditer(pattern, text)):
        # filename = m.group(1)
        # linenum = m.group(2)
        start, end = m.span()
        mark_text = text[start:end].replace('\n', '').replace('\0', '')
        # The empty dictionary below will be available as groupdicts
        # in handle_result() and can contain string keys and arbitrary JSON
        # serializable values.
        yield Mark(idx, start, end, mark_text, {})


def handle_result(args, data, target_window_id, boss, extra_cli_args, *a):
    # This function is responsible for performing some
    # action on the selected text.
    # matches is a list of the selected entries and groupdicts contains
    # the arbitrary data associated with each entry in mark() above
    matches, groupdicts = [], []
    for m, g in zip(data['match'], data['groupdicts']):
        if m:
            matches.append(m), groupdicts.append(g)
    cwd=data['cwd']
    for word, match_data in zip(matches, groupdicts):
        # Lookup the word in a dictionary, the open_url function
        # will open the provided url in the system browser
        # boss.show_error("","hello"+cwd)
        if word.startswith("http://") or word.startswith("https://") :
            boss.open_url(word)
        if word.startswith("/"):
            boss.launch("open-with",word)
        else:
            # boss.open_url(word)
            # https://github.com/kovidgoyal/kitty/blob/master/kitty/boss.py#L2470
            boss.launch("open-with",cwd+"/"+word)
            # boss.open_url(f'https://www.google.com/search?q=define:{word},{cwd},{match_data}')
