from pyvis import network as pvnet


def plot_g_pyviz(G, name='out.html', height='300px', width='500px'):
    g = G.copy() # some attributes added to nodes
    net = pvnet.Network(notebook=True, directed=True, height=height, width=width)
    opts = '''
        var options = {
          "physics": {
            "forceAtlas2Based": {
              "gravitationalConstant": -100,
              "centralGravity": 0.11,
              "springLength": 100,
              "springConstant": 0.09,
              "avoidOverlap": 1
            },
            "minVelocity": 0.75,
            "solver": "forceAtlas2Based",
            "timestep": 0.22
          }
        }
    '''

    net.set_options(opts)
    # uncomment this to play with layout
    # net.show_buttons(filter_=['physics'])
    net.from_nx(g)
    return net.show(name)
