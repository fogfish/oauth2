import React from 'react'
import { Flex, Box } from 'reflexbox'
import { useOAuth2, WhileIO } from 'components/OAuth2'
import Header from './Header'
import Registry from '../Registry'
import Issue from '../Issue'

const IO = WhileIO(undefined, Issue, Registry)

const UI = () => {
  const status = useOAuth2()
  return (<IO status={status} />)
}

const Account = () => (
  <>
    <Header />
    <Flex>
      <Box width={[1]} m="1em">
        <UI />
      </Box>
    </Flex>
  </>
)

export default Account
