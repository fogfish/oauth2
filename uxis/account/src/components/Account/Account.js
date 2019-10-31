import React from 'react'
import { Flex, Box } from 'reflexbox'
import Header from './Header'
import { Registry } from '../Registry'

const Account = (props) => (
  <>
    <Header />
    <Flex>
      <Box width={[1]} m={'1em'}>
        <Registry { ...props } />
      </Box>
    </Flex>
  </>
)

export default Account
