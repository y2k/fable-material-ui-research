import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';

export default {
    input: 'bin/Library.js',
    external: [
        'react',
        '@material-ui',
        '@material-ui/core',
        '@material-ui/utils',
        '@material-ui/styles',
        '@material-ui/system',
        '@material-ui/types',
        ],
    output: {
        file: 'bin/bundle.js',
        format: 'iife',
        name: 'MyModule'
        // globals: {
        //     react: 'React',
        //     'react-dom': 'ReactDOM',
        //     'Button': '@material-ui/core/Button'
        // }
    },
    plugins: [
        resolve(),
        commonjs({
            namedExports: {
                'node_modules/react/index.js': [
                    'cloneElement',
                    'createContext',
                    'Component',
                    'createElement',
                    'isValidElement',
                    'Children'
                ],
                'node_modules/react-dom/index.js': [
                    'render',
                    'hydrate'
                ],
                'node_modules/react-is/index.js': [
                    'isElement',
                    'isValidElementType',
                    'ForwardRef'
                ],
                'node_modules/@material-ui/core/styles/colorManipulator.js': [
                    'convertHexToRGB',
                    'rbgToHex'
                ],
            }
        })
        //     commonjs({
        //         // include: 'node_modules/**',
        //         namedExports: {
        //             'node_modules/react-is/index.js': ['isForwardRef', 'isValidElementType'],
        //         }
        //     })
    ]
};
