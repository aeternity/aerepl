link_node_app () {
    if ! [ -d $REBAR_BUILD_DIR/lib/$1 ]
    then
       ln -sd $PWD/node/_build/dev1/lib/$1 $REBAR_BUILD_DIR/lib/$1;
    fi
}

link_node_app aechannel
link_node_app aecontract
link_node_app aecore
link_node_app aefate
link_node_app aens
link_node_app aeoracle
link_node_app aeprimop
link_node_app aetx
link_node_app aeutils
link_node_app setup
