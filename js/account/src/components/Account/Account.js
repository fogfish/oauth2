import React from 'react'
import { Flex, Box } from 'reflexbox'
import { useOAuth2 } from 'components/OAuth2'
import { WhileIO } from 'components/WhileIO'
import Header from './Header'
import Error from './Error'
import { Registry } from '../Registry'

const IO = WhileIO(undefined, Error, Registry)

const UI = () => {
  const status = useOAuth2()
  return (<IO { ...status } />)
}

const Account = () => (
  <>
    <Header />
    <Flex>
      <Box width={[1]} m={'1em'}>
        <UI />
      </Box>
    </Flex>
  </>
)

export default Account
