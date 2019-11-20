import React from 'react'
import { Flex, Box } from 'reflexbox'
import { Card, Elevation, Divider } from '@blueprintjs/core'

const Head = ({ icon, title }) => (
  <h1 className="bp3-heading">
    <i className={`fa ${icon}`} aria-hidden="true"></i>
    &nbsp; &nbsp; {title}
  </h1>
)

const Foot = ({ Actions, Links }) => (
  <Box p={'2em'} style={{backgroundColor: '#F5F8FA', borderTop: '1px solid #EBF1F5'}}>
    <Flex justifyContent="space-between" mb={'1em'}>
      <Actions />
    </Flex>
    {Links &&
      <>
        <Divider />
        <Flex justifyContent="flex-end" mt={'1em'}>
          <Links />
        </Flex>
      </>
    }
  </Box>  
)

export const Dialog = ({ url, children, ...props }) => (
  <Flex alignItems="center" justifyContent="center">
    <Box width={[1, 27/40, '35em']} m={['0em', '2em']}>
      <Card elevation={Elevation.FOUR} style={{padding: 0}}>
        <form className="form-group" action={url} method="post">
          <Box p={['2em', '2em']}>
            <Head { ...props } />
            <Box mt={['3em', '3em']}>{children}</Box>
          </Box>
          <Foot { ...props } />
        </form>
      </Card>
    </Box>
  </Flex>
)
