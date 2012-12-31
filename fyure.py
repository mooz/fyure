#!/usr/bin/env python
# -*- coding: utf-8 -*-

import MeCab
import sys

# Inspired by http://d.hatena.ne.jp/cou929_la/20120917/1347860517

def mecab_parse(text, mecab):
    token_node = mecab.parseToNode(text)
    while token_node:
        yield token_node
        token_node = token_node.next

def register_token(grouped_tokens, token, byte_position):
    features = token.feature.split(",")

    hyoki = token.surface
    hinshi = features[0]
    yomi = features[-2]

    # skip
    if hinshi == "記号" or yomi == "*" or yomi == "":
        return

    token_kind = (hinshi, yomi)
    # print token_kind
    if not grouped_tokens.has_key(token_kind):
        grouped_tokens[token_kind] = {} # { surface -> [(byte_start, byte_end), (byte_start, byte_end), ...], ... }

    if not grouped_tokens[token_kind].has_key(hyoki):
        grouped_tokens[token_kind][hyoki] = [] # [(byte_start, byte_end), (byte_start, byte_end), ...]

    token_end_byte = byte_position + token.rlength
    token_begin_byte = token_end_byte - token.length
    grouped_tokens[token_kind][hyoki].append((token_begin_byte, token_end_byte))

def compute_grouped_tokens(string, mecab):
    grouped_tokens = {}
    byte_position = 0
    for token in mecab_parse(string, mecab):
        if token.surface:
            register_token(grouped_tokens, token, byte_position)
        byte_position += token.rlength
    return grouped_tokens

def escape_double_quote(string):
    return '"' + string.replace('"', r'\"') + '"'

def grouped_tokens_to_sexp(grouped_tokens):
    sexp = "("
    for (hinshi, yomi), same_kind_hyoki_container in grouped_tokens.iteritems():
        sexp += "(" + escape_double_quote(yomi + "[" + hinshi + "]") + " . ("
        for hyoki, positions in same_kind_hyoki_container.iteritems():
            sexp += "(" + escape_double_quote(hyoki) + " . " + \
                    "(" + " ".join("(%d . %d)" % (start, end) for (start, end) in positions) + "))"
        sexp += "))\n"
    sexp += ")"
    return sexp

def print_candidates(input_stream = sys.stdin, output_stream = sys.stdout):
    mecab_arg = ""
    if len(sys.argv) > 1:
        mecab_arg += "-d " + sys.argv[1]
    mecab = MeCab.Tagger(mecab_arg)

    grouped_tokens = compute_grouped_tokens(input_stream.read(), mecab)

    duplicated_grouped_tokens = {}
    for key, same_kind_hyoki_container in grouped_tokens.iteritems():
        if len(same_kind_hyoki_container) >= 2:
            duplicated_grouped_tokens[key] = same_kind_hyoki_container

    output_stream.write(grouped_tokens_to_sexp(duplicated_grouped_tokens))

if __name__ == "__main__":
    print_candidates()
