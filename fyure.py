#!/usr/bin/env python
# -*- coding: utf-8 -*-

import MeCab
import sys

# TODO: allow users to specify dictionary path
def mecab_parse(text, mecab = MeCab.Tagger("-Ochasen")):
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
    if hinshi == "記号" or yomi == "*":
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

def compute_grouped_tokens(string):
    grouped_tokens = {}
    byte_position = 0
    for token in mecab_parse(string):
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

if __name__ == "__main__":
    grouped_tokens = compute_grouped_tokens(sys.stdin.read())

    duplicated_grouped_tokens = {}
    for key, same_kind_hyoki_container in grouped_tokens.iteritems():
        if len(same_kind_hyoki_container) >= 2:
            duplicated_grouped_tokens[key] = same_kind_hyoki_container

    print grouped_tokens_to_sexp(duplicated_grouped_tokens)
