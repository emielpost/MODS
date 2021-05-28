#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May 18 17:40:07 2021

@author: emielpost
"""

def plot_flat_graph(output_dir, threshold):
    import pygraphviz as pgv
    import pandas as pd
    
    edge_counts = pd.read_csv(output_dir + 'edge_counts.txt', 
                              sep = ' ', header = None)
    no_runs = pd.read_csv(output_dir + 'no_runs.txt', header = None)[0][0]
    
    # construct edge count dataframe
    split_edges = edge_counts[0].str.split('.', expand=True)
    nodes = split_edges[0].str.split(',', expand=True)
    node1_wo_tag = [edge[0] for edge in nodes[0].str.split("_")]
    node2_wo_tag = [edge[0] for edge in nodes[1].str.split("_")]
    orientations = split_edges[1].str.split(',', expand=True)
    
    edges = pd.DataFrame({'node1': node1_wo_tag, 'node2': node2_wo_tag,
                          'tail': orientations[0], 'head': orientations[1],
                          'color': split_edges[2], 'style': split_edges[3],
                          'fraction': edge_counts[1]/no_runs})
    
    # Some duplicates present: combine and take mean fraction
    edges_final = edges.groupby(['node1','node2','tail', 'head',
                                 'color', 'style']).mean().round(3)
    edges_final = edges_final.reset_index()
    
    # tiers
    tiers = pd.read_csv(output_dir + 'knowledge.txt', sep = ' ', skiprows = 1, header = None)
    tiers_dict = tiers.set_index(0).transpose().to_dict()
    
    # construct graph    
    G = pgv.AGraph(directed=True, strict=False)
    G.graph_attr['center'] = '1'
    G.graph_attr['sep'] = "1"
    G.graph_attr['size'] = "10,10"
    G.graph_attr['start'] = str(123)  # deterministic neato layout algorithm
    G.graph_attr['notranslate'] = 'true'
    G.graph_attr['splines'] = 'curved'

    # initialize nodes    
    temporal_vars = [var for tier in tiers_dict.values() for var in tier.values()]
    unique_nodes = set([var.split('_')[0] for var in temporal_vars])

    organ_vars = ['Circ', 'Coag', 'Liver', 'Renal', 'Resp', 'GIS']
    for node in unique_nodes:
        if node in organ_vars:
            G.add_node(node, style='filled', fillcolor='grey')
        else:
            G.add_node(node)

    # initialize edges
    class AutoVivification(dict):
        def __missing__(self, key):
            value = self[key] = type(self)()
            return value

        def __add__(self, x):
            """ override addition when self is empty """
            if not self:
                return 0 + x
            raise ValueError

        def __sub__(self, x):
            """ override subtraction when self is empty """
            if not self:
                return 0 - x
            raise ValueError

    new_dict = AutoVivification()
    for _,row in edges_final.iterrows():
        node1 = row['node1']
        node2 = row['node2']
        specs = row['tail'] + '_' + row['head'] + '_' + \
                row['color'] + '_' + row['style'] + '_' + \
                "{0:.2f}".format(row['fraction'])
        new_edge = node1.split('_')[0], node2.split('_')[0]
        new_edge_rev = node2.split('_')[0], node1.split('_')[0]

        
        if new_edge_rev in new_dict: # in some bootsrap runs edges may be reversed
            specs_rev = row['head'] + '_' + row['tail'] + '_' + \
                row['color'] + '_' + row['style'] + '_' + \
                "{0:.2f}".format(row['fraction'])
            new_dict[new_edge_rev][specs_rev] += 1
        else:
            new_dict[new_edge][specs] += 1
    
    if no_runs == 1:        
        for edge, specs in new_dict.items():
            for spec in specs.keys():
                node1, node2 = edge
                tail, head, color, style, fraction = spec.split('_')
                
                G.add_edge(node1, node2,
                           arrowtail=tail,
                           arrowhead=head,
                           color = color,
                           style = style,
                           dir='both')
        G.layout('neato', '-s72 -Goverlap=scale')
        G.draw(output_dir + '/flat_single' + '.pdf')
        G.write(output_dir + '/flat_single.dot')
            
    else:
        for edge, specs in new_dict.items():
            for spec in specs.keys():
                node1, node2 = edge
                tail, head, color, style, fraction = spec.split('_')

                if float(fraction) >= threshold:
                    G.add_edge(node1, node2,
                               arrowtail=tail,
                               arrowhead=head,
                               color = color,
                               style = style,
                               label = fraction,
                               dir='both')
                
        G.layout('neato', '-s72 -Goverlap=scale')
        G.draw(output_dir + '/flat_boot' + '_' + str(threshold).replace('.','') + '.pdf')

        try:
            G_single = pgv.AGraph(output_dir + '/flat_single.dot')
            
            for edge in G_single.iteredges():
                node1, node2 = edge
                tail = edge.attr['arrowtail']
                head = edge.attr['arrowhead']
                color = edge.attr['color']
                style = edge.attr['style']

                try:
                    row = edges_final[(edges_final['node1'] == node1) &
                                      (edges_final['node2'] == node2) &
                                      (edges_final['tail'] == tail) &
                                      (edges_final['head'] == head) &
                                      (edges_final['color'] == color) &
                                      (edges_final['style'] == style)]
                    label = row['fraction'].item()
                except:
                    try: # edges may have been reversed in some boot runs
                        row = edges_final[(edges_final['node1'] == node2) &
                                          (edges_final['node2'] == node1) &
                                          (edges_final['tail'] == head) &
                                          (edges_final['head'] == tail) &
                                          (edges_final['color'] == color) &
                                          (edges_final['style'] == style)]
                        label = row['fraction'].item()
                    except: # or never present in any of the runs
                        label = 0.00

                edge.attr['label'] = "{0:.2f}".format(label)
            
            G_single.layout('neato', '-s72 -Goverlap=scale')
            G_single.draw(output_dir + '/flat_single_adjusted' + '.pdf')
        except:
            print('no single run available')    
    return G


def plot_dynamic_graph(output_dir, threshold):
    import pygraphviz as pgv
    import pandas as pd
    
    edge_counts = pd.read_csv(output_dir + 'edge_counts.txt', 
                              sep = ' ', header = None)
    no_runs = pd.read_csv(output_dir + 'no_runs.txt', header = None)[0][0]
    
    # construct edge count dataframe
    split_edges = edge_counts[0].str.split('.', expand=True)
    nodes = split_edges[0].str.split(',', expand=True)
    orientations = split_edges[1].str.split(',', expand=True)
    
    edges_final = pd.DataFrame({'node1': nodes[0], 'node2': nodes[1],
                                'tail': orientations[0], 'head': orientations[1],
                                'color': split_edges[2], 'style': split_edges[3],
                                'fraction': edge_counts[1]/no_runs})
    
    # tiers
    tiers = pd.read_csv(output_dir + 'knowledge.txt', sep = ' ', skiprows = 1, header = None)
    tiers_dict = tiers.set_index(0).transpose().to_dict()
    
    G = pgv.AGraph(directed=True, strict=False)
    G.graph_attr['center'] = '1'
    G.graph_attr['splines'] = "curved"
    G.graph_attr['sep'] = ".2"
    G.graph_attr['size'] = "10,10"

    # add nodes
    organ_vars = ['Circ', 'Coag', 'Liver', 'Renal', 'Resp', 'GIS']
    for tier_no, nodes in tiers_dict.items():
        for node_no, node in nodes.items():
            pos_x = str(7.5 * (node_no + 1))
            pos_y = str(5.0 + 10*tier_no)
            var = node.split('_')[0]

            if var in organ_vars:
                G.add_node(node, pos=pos_x + "," + pos_y, pin='true', style='filled', fillcolor='grey')
            else:
                G.add_node(node, pos=pos_x + "," + pos_y, pin='true')
    
    # add edges
    if no_runs == 1:
        for _,row in edges_final.iterrows():
            G.add_edge(row['node1'], row['node2'],
                       arrowtail=row['tail'],
                       arrowhead=row['head'],
                       color = row['color'],
                       style = row['style'],
                       dir='both')
            
        G.layout('neato', '-s72 -Goverlap=scale')
        G.draw(output_dir + '/dynamic_single' + '.pdf')
        G.write(output_dir + '/dynamic_single.dot')
    else:
        for _,row in edges_final.iterrows():
            if float(row['fraction']) >= threshold:
                G.add_edge(row['node1'], row['node2'],
                           arrowtail=row['tail'],
                           arrowhead=row['head'],
                           color = row['color'],
                           style = row['style'],
                           label = "{0:.2f}".format(row['fraction']),
                           dir='both')
            
        G.layout('neato', '-s72 -Goverlap=scale')
        G.draw(output_dir + '/dynamic_boot' + '_' + str(threshold).replace('.','') + '.pdf')
        
        try:
            G_single = pgv.AGraph(output_dir + '/dynamic_single.dot')
            
            for edge in G_single.iteredges():
                node1, node2 = edge
                tail = edge.attr['arrowtail']
                head = edge.attr['arrowhead']
                color = edge.attr['color']
                style = edge.attr['style']
                
                try: 
                    row = edges_final[(edges_final['node1'] == node1) &
                                      (edges_final['node2'] == node2) & 
                                      (edges_final['tail'] == tail) &
                                      (edges_final['head'] == head) &
                                      (edges_final['color'] == color) &
                                      (edges_final['style'] == style)]
                    label = row['fraction'].item()
                    
                except:
                    try: # edges may have been reversed in some boot runs
                        row = edges_final[(edges_final['node1'] == node2) &
                                          (edges_final['node2'] == node1) & 
                                          (edges_final['tail'] == head) &
                                          (edges_final['head'] == tail) &
                                          (edges_final['color'] == color) &
                                          (edges_final['style'] == style)]
                        label = row['fraction'].item()
                        
                    except: # or never present in any of the runs
                        label = 0.00
                        
                edge.attr['label'] = "{0:.2f}".format(label)
            G_single.layout('neato', '-s72 -Goverlap=scale')
            G_single.draw(output_dir + '/dynamic_single_adjusted' + '.pdf')
        except:
            print('no single run available')    
    return G
