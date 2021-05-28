#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed May 26 20:23:59 2021

@author: emielpost
"""
def flat2SEM(output_dir):
    import pygraphviz as pgv
    import pandas as pd
    
    G_single = pgv.AGraph(output_dir + '/flat_single.dot')
    labels = pd.read_csv(output_dir + '/SEM.txt', sep = ' ')
    
    df = pd.DataFrame(columns = ['node1','node2','label'])
    for _,row in labels.iterrows():
        node1 = row['lhs'].split('_')[0]
        node2 = row['rhs'].split('_')[0]
        label = row['std.all']
        
        new_row = pd.DataFrame([[node1, node2, label]], 
                               columns = ['node1','node2','label'])
        df = df.append(new_row)
    
    labels_new = df.groupby(['node1','node2']).mean().round(3).reset_index()
    
    for edge in G_single.iteredges():
        node1, node2 = edge
        
        selection = labels_new[(labels_new['node1'] == node1) & (labels_new['node2'] == node2)]
        if selection.empty:
            selection = labels_new[(labels_new['node1'] == node2) & (labels_new['node2'] == node1)]
            edge.attr['label'] = selection['label'].item()
        else:
            edge.attr['label'] = selection['label'].item()
        
    G_single.layout('neato', '-s72 -Goverlap=scale')
    G_single.draw(output_dir + '/flat_single_SEM' + '.pdf')

    return G_single

def single2SEM(output_dir):
    import pygraphviz as pgv
    import pandas as pd
    
    G_single = pgv.AGraph(output_dir + '/dynamic_single.dot')
    labels = pd.read_csv(output_dir + '/SEM.txt', sep = ' ')
    labels['std.all'] = labels['std.all'].round(3)
    
    for edge in G_single.iteredges():
        node1, node2 = edge
        
        selection = labels[(labels['lhs'] == node1) & (labels['rhs'] == node2)]
        if selection.empty:
            selection = labels[(labels['lhs'] == node2) & (labels['rhs'] == node1)]
            edge.attr['label'] = selection['std.all'].item()
        else:
            edge.attr['label'] = selection['std.all'].item()
        
    G_single.layout('neato', '-s72 -Goverlap=scale')
    G_single.draw(output_dir + '/dynamic_single_SEM' + '.pdf')

    return G_single



