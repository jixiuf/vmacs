import os
use_origin_frq=False             # 使用原始词库的词频 （否则用知频.txt进行更新）

frost_path="~/repos/rime-frost/"
word_freq_path = frost_path+"others/知频.txt"

dicts_src_dir=os.path.join(frost_path, "cn_dicts")
file_list = ['8105.dict.yaml', '41448.dict.yaml', 'base.dict.yaml', 'ext.dict.yaml', 'others.dict.yaml']
output='../dicts/wb86_cn_dicts'

# dicts_src_dir=os.path.join("../dicts/wangxiang_cn_dicts")
# file_list = ['base.dict.yaml', 'associational.dict.yaml', 'corrections.dict.yaml',
#              'poetry.dict.yaml','place.dict.yaml','compatible.dict.yaml',
#              'chars.dict.yaml', 'correlation.dict.yaml']
# output='../dicts/wb86_wangxiang_cn_dicts'


wb_86_dict_list = ['../dicts/wubi86/wubi.dict.yaml']
os.makedirs(output, exist_ok=True)
# Function to read a file
def read_file(file_path):
    with open(file_path, 'r', encoding='utf-8') as file:
        return file.read()

# Function to write content to a file
def write_file(file_path, content):
    with open(file_path, 'w', encoding='utf-8') as file:
        file.write(content)

# Function to update missing encodings in the file
def update_missing_encodings(file_path, write_file_path, dict_data):
    # Read the file content
    file_content = read_file(file_path)
    # Split the content into lines
    lines = file_content.split('\n')
    # Create an updated content variable
    updated_content = ''

    char_map = {}
    # Process each line
    for line in lines:
        if '\t' not in line or line.startswith("#"):
            updated_content += line + '\n'
            continue

        tokens= line.split('\t')
        char_list = tokens[0]
        if len(tokens) >2:
            freq= tokens[2]

        if char_list in char_map:
            continue

        lack_flag = False
        for char in char_list:
            if char not in dict_data:
                #print("缺失"+char)
                lack_flag = True
                continue
        if lack_flag:
            continue

        word_encode_list = []
        for char in char_list:
            if char not in dict_data:
                print("缺失"+char)
                continue
            dict_encode_list = dict_data[char]
            dict_encode = ';'.join(dict_encode_list)
            word_encode_list.append(dict_encode)

        word_encode = ' '.join(word_encode_list)
        if not use_origin_frq :
            if char_list in word_freq:
                freq = word_freq[char_list]
            else:
                freq = 0
        updated_line = f"{char_list}\t{word_encode}\t{freq}"
        updated_content += updated_line + '\n'
        char_map[char_list] = ''

    # Write the updated content back to the file
    write_file(write_file_path, updated_content)

# 载入频率
freq_file = open(os.path.expanduser(word_freq_path), 'r', encoding='utf-8')
word_freq = {}
for line in freq_file:
    line = line.strip()
    params = line.split("\t")
    word = params[0]
    freq = int(params[1])

    if word in word_freq:
        word_freq[word] += freq
    else:
        word_freq[word] = freq

dict_data = {}
for wb_86_dict in wb_86_dict_list:
    with open('./'+wb_86_dict, 'r', encoding='utf-8') as dict_file:
        for line in dict_file:
            if "\t" in line and not line.startswith("#"):

                params = line.strip().split('\t')
                if wb_86_dict == '../dicts/wubi86/wubi.dict.yaml' and len(params) != 4:
                    continue
                character = params[0]
                if len(character) !=1:
                    continue

                if wb_86_dict == '../dicts/wubi86/wubi.dict.yaml':
                    encoding = params[3]

                encode_left = encoding[0:2]
                encode_right = encoding[2:]
                if len(encode_right) == 1:
                    encode_right = encode_right + '0'

                encoding = encode_left + ',' + encode_right

                if character not in dict_data:
                    dict_data[character] = [encoding]
                else:
                    if encoding not in dict_data[character]:
                        dict_data[character].append(encoding)

with open('../dicts/wubi86/wubi86.dict.yaml', 'r', encoding='utf-8') as dict_file:
    for line in dict_file:
        if "\t" in line and not line.startswith("#"):

            params = line.strip().split('\t')
            if len(params) != 4:
                continue
            character = params[0]
            if len(character) !=1:
                continue
            encoding = params[1]
            if len(encoding) != 1:
                continue

            encoding = encoding +'0,00'
            # print(line)

            if character not in dict_data:
                dict_data[character] = [encoding]
            else:
                add_flag = True
                for exist_encode in dict_data[character]:
                    if exist_encode.startswith(encoding):
                        add_flag = False
                if add_flag:
                    dict_data[character].append(encoding)

with open('../dicts/wubi86/wubi86.dict.yaml', 'r', encoding='utf-8') as dict_file:
    for line in dict_file:
        if "\t" in line and not line.startswith("#"):

            params = line.strip().split('\t')
            if len(params) == 4:
                continue
            character = params[0]
            if len(character) !=1:
                continue
            encoding = params[1]

            encode_left = encoding[0:2]
            encode_right = encoding[2:]
            if len(encode_right) == 1:
                encode_right = encode_right + '0'

            if character not in dict_data:
                dict_data[character] = [encoding]



print(dict_data['巴'])
print(dict_data['不'])
print(dict_data['𩽾'])

for file_name in file_list:
    # File paths
    cn_dicts_path = os.path.expanduser(dicts_src_dir)
    yaml_file_path = os.path.join(cn_dicts_path, file_name)
    write_file_path = os.path.join(output, file_name)

    print(yaml_file_path)
    # Update missing encodings in the file
    update_missing_encodings(yaml_file_path, write_file_path, dict_data)
