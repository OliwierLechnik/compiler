import json
import sys
import os
from graphviz import Digraph

def draw_cfg(json_file, output_file):
    if not os.path.exists(json_file):
        print(f"Error: {json_file} not found.")
        return

    with open(json_file, 'r') as f:
        data = json.load(f)

    # Create Digraph
    # 'dot' engine is best for hierarchical/flow layouts
    dot = Digraph(comment='CFG', format='png')
    dot.attr(rankdir='TB')  # Top to Bottom
    dot.attr('node', fontname='Consolas', fontsize='10')

    # Iterate over blocks
    for block in data['blocks']:
        node_id = block['id']
        
        # 1. Build Phi Nodes HTML (Top Section)
        # Using a distinct background color (e.g., cornsilk) for SSA merges
        phi_rows = ""
        phis = block.get('phis', [])
        for phi in phis:
            clean_phi = phi.replace('<', '&lt;').replace('>', '&gt;')
            phi_rows += f'<tr><td align="left" bgcolor="cornsilk">{clean_phi}</td></tr>'

        # 2. Build Instructions HTML (Middle Section)
        instr_rows = ""
        for instr in block['instructions']:
            clean_instr = instr.replace('<', '&lt;').replace('>', '&gt;')
            instr_rows += f'<tr><td align="left" balign="left">{clean_instr}</td></tr>'

        # 3. Build Terminator HTML (Bottom Section)
        term_clean = block['terminator'].replace('<', '&lt;').replace('>', '&gt;')
        
        # Construct the HTML Label (UML Style)
        # Port "f0" is header, "f1" instructions, "f2" terminator
        label = f'''<<table border="0" cellspacing="0" cellborder="1">
            <tr><td bgcolor="lightblue"><b>{node_id}</b></td></tr>
            {phi_rows}
            {instr_rows}
            <tr><td bgcolor="lightgrey"><i>{term_clean}</i></td></tr>
        </table>>'''

        # Add node with "plain" shape so the HTML table controls the look
        dot.node(node_id, label=label, shape='plain')

        # Add Edges
        for edge in block['edges']:
            target = edge['target']
            tag = edge['label']
            
            # Styling edges
            color = 'black'
            style = 'solid'
            
            if tag == 'true': 
                color = 'green'
            elif tag == 'false': 
                color = 'red'
            elif tag == 'call':
                color = 'blue'
                style = 'dashed'
            
            dot.edge(node_id, target, label=tag, color=color, fontcolor=color, style=style)

    # Render
    output_path = dot.render(output_file, view=True)
    print(f"Graph rendered to {output_path}")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python draw_graph.py <path_to_cfg.json>")
    else:
        # Output filename defaults to 'cfg_output'
        draw_cfg(sys.argv[1], 'cfg_output')